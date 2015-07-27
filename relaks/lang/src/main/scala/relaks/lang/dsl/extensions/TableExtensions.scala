package relaks.lang.dsl.extensions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.core.JsonParser.Feature
import com.fasterxml.jackson.databind.{SerializationFeature, ObjectMapper}
import com.fasterxml.jackson.databind.module.SimpleModule
import com.typesafe.scalalogging.LazyLogging
import org.apache.calcite.rel.core.JoinRelType
import org.apache.drill.common.JSONOptions
import org.kiama.attribution.Attribution
import org.kiama.relation.GraphTree
import org.apache.drill.common.expression.{SchemaPath, LogicalExpression, FieldReference}
import org.apache.drill.common.logical.data.{Transform => DrillTransform, Join => DrillJoin, NamedExpression, JoinCondition, LogicalOperator, Scan}
import org.kiama.==>
import org.kiama.rewriting.{Rewriter, Strategy}
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.AST.syntax._
import relaks.lang.dsl._
import relaks.lang.dsl.extensions.ast._
import relaks.lang.dsl.utils.TreePrettyPrintable
import relaks.lang.impl.Row
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.ops.tuple.ToTraversable
import shapeless.ops.{hlist, tuple}

import scala.collection.mutable
import scala.language.{existentials, implicitConversions}
import scalaz.Scalaz._
import scalaz._

/**
 * Created by Pietras on 17/05/15.
 */
trait TableExtensions extends TableIO with TableOps with TableUtils {

}

trait TableUtils extends Symbols with Queries {
//  sealed trait ForTableQuery
//  private def forTable(sym: Sym): Sym @@ ForTableQuery = ForTable.unapply(sym).get
//
//  private object ForTable {
//    def unapply(sym: Sym) = sym match {
//      case Some(s) /> (_: SourceQuery) => Tag[Sym, ForTableQuery](s).some
//      case _ => None
//    }
//  }

  //note that it only works on syms
  //it also does not extract nested comprehensions
  object ComprehensionBuilder extends Attribution {
    val comprehension: Expression => Option[Comprehension] = attr {
      case Some(query) /> StepTable(next) =>
        val _ /> inner = query
        (next match {
          case Some(q) /> SourceTable(_) => Comprehension(q).some
          case _ => comprehension(next)
        }).map { comprehension =>
          inner match {
            case (expr: Filter) => comprehension.copy(filter = expr +: comprehension.filter)
            case (expr: Transform) => comprehension.copy(transform = expr +: comprehension.transform)
            case (expr: Limit) => comprehension.copy(limit = expr +: comprehension.limit)
            case (expr: GroupBy) => comprehension.copy(groupBy = expr +: comprehension.groupBy)
            case (expr: OrderBy) => comprehension.copy(orderBy = expr +: comprehension.orderBy)
          }
        }
      case _ => None
    }
  }
}

trait TableOps extends Symbols with Queries {

  type RowN[L <: HList] = Rep[Tup[L]]
  type Row[A] = Rep[Tup[A :: HNil]]
  type Row2[A, B] = Rep[Tup[A :: B :: HNil]]
  type Row3[A, B, C] = Rep[Tup[A :: B :: C :: HNil]]


  class ProjectedTableComprehensions[FieldsLen <: Nat](private[extensions] val fields: Vector[Symbol],
                                                       private[extensions] val query: Atom)
    extends Rep[Table] with TableComprehensions {

    override type ComprehensionsMonad = ProjectedTableComprehensions[FieldsLen]
    override val tree: Expression = query

    private def tupleGenerator[F <: HList](filterFields: Vector[Symbol] = fields): (Generator, Rep[Tup[F]]) = {
      val gen = Generator.fromFields(fields)
      val tuple = gen.toTuple[F] //todo types
      (gen, tuple)
    }

    def flatMap[F <: HList](f: Rep[Tup[F]] => Rep[Table])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]): Rep[Table] = {
      val (gen, tuple) = tupleGenerator()
      val mapper = f(tuple)

      //TODO this could return a typed table
      val expr = Transform(gen, query, mapper.tree)(new UntypedTableType)
      new Rep[Table] {
        override val tree: Atom = expr  //TODO this must be an atom for some reason (probably because of the lack of sym memoization)
      }
    }

    def map[F <: HList, T <: HList](f: Rep[Tup[F]] => Rep[Tup[T]])(implicit lenEv: hlist.Length.Aux[F, FieldsLen], lenOutEv: hlist.Length[T]) = {
      val (gen, tuple) = tupleGenerator()
      val result = f(tuple)
      val mapper = RowRep(result)

      val _/>TupleWithNames(_, names) = result.tree

      //TODO this could return a typed table
      val expr = Transform(gen, query, mapper.tree)(new UntypedTableType)
      new ProjectedTypedTableComprehensions[T](names.map(Symbol.apply), expr)
    }

    def withFilter(f: Rep[Tup[Nothing]] => Rep[Boolean]) = {
      val (generator, rep) = tupleGenerator()
      val cond = f(rep).tree
      cond match {
        case _/>Literal(true) => this //remove empty filters immediately, most likely occurs
                                   // with the cast matcher _.isInstanceOf in for comprehensions
        case _ => createFilterComprehension(generator, cond)
      }
    }

    private def createFilterComprehension(generator: Generator, cond: Expression): ProjectedTableComprehensions[FieldsLen] = {
      val filteredTable = Filter(generator, query, cond)
      new ProjectedTableComprehensions[FieldsLen](fields, filteredTable)
    }

    override protected[extensions] def filterImpl[F <: HList](f: Rep[Tup[F]] => Rep[Boolean], fields: Vector[Symbol]) = {
      val (generator, rep) = tupleGenerator()
      val cond = f(rep).tree
      createFilterComprehension(generator, cond)
    }

    def filter[F <: HList](f: Rep[Tup[F]] => Rep[Boolean])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      filterImpl(f, fields)
    }

    override protected[extensions] def orderImpl(fieldsVec: Vector[Symbol], expr: OrderBy): ProjectedTableComprehensions[FieldsLen] = {
      new ProjectedTableComprehensions[FieldsLen](fieldsVec, expr)
    }
  }

  class ProjectedTypedTableComprehensions[H <: HList](fields: Vector[Symbol], query: Atom)
                                                                (implicit val lenEnv: hlist.Length[H])
    extends Rep[Table] with TableComprehensions {

    override val tree: TTree = query

    implicit val lenEnvAux = new hlist.Length[H] {
      override type Out = lenEnv.Out
      override def apply(): lenEnv.Out = lenEnv.apply()
    }

    lazy val untypedComprehension = new ProjectedTableComprehensions[lenEnv.Out](fields, query)

    def flatMap(f: Rep[Tup[H]] => Rep[Table]) = untypedComprehension.flatMap(f)
    def map[T <: HList](f: Rep[Tup[H]] => Rep[Tup[T]])(implicit lenEnv: hlist.Length[T]) = untypedComprehension.map(f)
    val withFilter = (untypedComprehension.withFilter _) andThen fromUntypedTableComprehension //TODO this does not enforce types
    def filter(f: Rep[Tup[H]] => Rep[Boolean]) = fromUntypedTableComprehension(untypedComprehension.filter(f))

    override type ComprehensionsMonad = ProjectedTypedTableComprehensions[H]

    override protected def filterImpl[F <: HList](f: (Rep[Tup[F]]) => Rep[Boolean], fields: Vector[Symbol]) =
      fromUntypedTableComprehension(untypedComprehension.filterImpl(f, fields))

    override protected def orderImpl(fieldsVec: Vector[Symbol], expr: OrderBy) =
      fromUntypedTableComprehension(untypedComprehension.orderImpl(fieldsVec, expr))

    private def fromUntypedTableComprehension(comprehensions: ProjectedTableComprehensions[lenEnv.Out]) =
      new ProjectedTypedTableComprehensions[H](comprehensions.fields, comprehensions.query)
  }

  trait OrderableTableComprehensions {
    type ComprehensionsMonad
    val tree: Expression
    protected def orderImpl(fieldsVec: Vector[Symbol], expr: OrderBy): ComprehensionsMonad

    //TODO ordering
    def orderBy[P <: Product](fields: P)(implicit toVector: ToTraversable.Aux[P, Vector, Symbol]): ComprehensionsMonad = {
      val fieldsVec = toVector(fields)
      val expr = OrderBy(tree, fieldsVec.map(FieldWithDirection(_, Asc)))(new UntypedTableType)
      orderImpl(fieldsVec, expr)
    }

    def orderBy(field: Symbol): ComprehensionsMonad = {
      orderBy(Tuple1(field))
    }
  }

  trait TableComprehensions extends Rep[Table] with OrderableTableComprehensions {
    type ComprehensionsMonad
    protected def filterImpl[F <: HList](f: Rep[Tup[F]] => Rep[Boolean], fields: Vector[Symbol]): ComprehensionsMonad
    def filter[P <: Product, FL <: Nat, F <: HList](fields: P)(f: Rep[Tup[F]] => Rep[Boolean])(implicit
                                                                                               lenEv: tuple.Length.Aux[P, FL],
                                                                                               lenEnv2: hlist.Length.Aux[F, FL],
                                                                                               toVector: ToTraversable.Aux[P, Vector, Symbol]) =
      filterImpl(f, toVector(fields))

  }

  class TableOperations(arg1: Rep[Table]) extends OrderableTableComprehensions {
    def apply[P <: Product, FieldsLen <: Nat](fields: P)(implicit /*tupEv: IsTuple[P],*/
                                                      lenEv: tuple.Length.Aux[P, FieldsLen],
                                                      fieldsLength: ToInt[FieldsLen],
                                                      toVector: ToTraversable.Aux[P, Vector, Symbol]) = {

      new ProjectedTableComprehensions[FieldsLen](toVector(fields), arg1.tree)
    }

    override type ComprehensionsMonad = Rep[Table]
    override protected def orderImpl(fieldsVec: Vector[Symbol], expr: OrderBy): ComprehensionsMonad = new Rep[Table] {
      override val tree: Atom = expr
    }

    override val tree: Expression = arg1.tree
  }

  implicit def addTableOps(t: Rep[Table]): TableOperations = new TableOperations(t)
}

trait TableIO extends Symbols {
  private[dsl] var storedOutput: Set[Expression] = Set.empty

  def load(path: String): Rep[Table] = new Rep[Table] {
    override val tree: Atom = LoadTableFromFs(path)(new UntypedTableType)
  }

  def load[S <: HList](path: String, schema: Rep[Tup[S]]): Rep[TypedTable[S]] = new Rep[TypedTable[S]] {
    override val tree: Atom = LoadTableFromFs(path)(schema.getTpe)
  }

  def store[T <: Table](table: Rep[T]): Unit = storedOutput += table.tree
}

trait BaseRelationalCompilers extends Symbols with Queries with TableUtils {

  sealed trait RelationalCompilerPartBase {

    type NodeType
    protected var emittedQueries: Map[Sym, NodeType] = Map.empty
    protected val queryEnv: Map[Sym, NodeType]
    protected def fromCache(sym: Sym) = queryEnv.get(sym).orElse(emittedQueries.get(sym))

    final protected def compileTable(expr: Expression): NodeType = expr match {
      case Some(sym) /> _ if fromCache(sym).isDefined =>
        val op = fromCache(sym).get
        emittedQueries += ((sym, op))
        op
      case Some(sym) /> Query(q) =>
        val op = compileQuery(q)
        emittedQueries += ((sym, op))
        op
    }

    def compileQuery(q: Query): NodeType
  }

  abstract class CompileRelational extends RelationalCompilerPartBase {
    def apply(root: Expression): Writer[Map[Sym, NodeType], NodeType] = {
      val result = compileTable(root)
      Writer(emittedQueries, result)
    }
  }
}

trait SQLCompilers extends BaseRelationalCompilers {
  trait SQLCompilerPart extends RelationalCompilerPartBase {
    override type NodeType = String

    override def compileQuery(q: Query): NodeType = q match {
      case LoadTableFromFs(path) =>
        s"$path"
      case Join((_, left), (_, right), typ, conditions) =>
        s"select * from (${compileTable(left)}) join (${compileTable(right)})"
      case Transform(gen: Generator, source, _/>Pure(_/>(select: TupleConstructor))) =>
        val sourceOp = compileTable(source)

        val env = gen.symsToFields.andThen(_.name)
        val transforms = compilePureRow(select)(env).zip(select.names).map
        { case (expr, name) => s"$expr as $name" }

        s"select ${transforms.mkString(", ")} from ($sourceOp)"
    }

    private def compilePureRow(tup: TupleConstructor): ((Sym => String) => Vector[String]) = (env: Sym => String) =>
      tup.tuple.map {
        case _ /> link => ???
        case s: Sym => s"""`${env(s)}`"""
      }
  }

  class CompileSQL(override val queryEnv: Map[Sym, String] = Map.empty)
    extends CompileRelational with SQLCompilerPart

}

trait DrillCompilers extends BaseRelationalCompilers with Symbols with Queries with LazyLogging with TableUtils {
  private lazy val mapper = {
    val m = new ObjectMapper
    val deserModule: SimpleModule = new SimpleModule("LogicalExpressionDeserializationModule")
      .addDeserializer(classOf[LogicalExpression], new LogicalExpression.De(null))
      .addDeserializer(classOf[SchemaPath], new SchemaPath.De(null))
    m.registerModule(deserModule)
    m.enable(SerializationFeature.INDENT_OUTPUT)
    m.configure(Feature.ALLOW_UNQUOTED_FIELD_NAMES, true)
    m.configure(JsonGenerator.Feature.QUOTE_FIELD_NAMES, true)
    m.configure(Feature.ALLOW_COMMENTS, true)
    m
  }

  trait DrillCompilerPart extends RelationalCompilerPartBase {
    override type NodeType = LogicalOperator

    override def compileQuery(q: Query): LogicalOperator = q match {
      case LoadTableFromFs(path) =>
        val optionsJson = "{\"format\" : {\"type\" : \"parquet\"},\"files\" : [ \"file:"  + path + "\" ]}"
        val opts = mapper.readValue(optionsJson, classOf[JSONOptions])
        val scan = new Scan("dfs", opts)
        scan

      case Join((_, left), (_, right), typ, conditions) =>
        val leftOp = compileTable(left)
        val rightOp = compileTable(right)

        val joinT = typ match {
          case CartesianJoin => JoinRelType.INNER
          case InnerJoin => JoinRelType.INNER
        }

        new DrillJoin(leftOp, rightOp, Array.empty[JoinCondition], joinT)

      case Transform(gen: Generator, source, _/>Pure(_/>(select: TupleConstructor))) =>
        val sourceOp = compileTable(source)

        val env = gen.symsToFields.andThen(_.name)
        val transforms = compilePureRow(select)(env).zip(select.names).map
        { case (expr, name) => new NamedExpression(mapper.readValue(expr, classOf[LogicalExpression]), new FieldReference(name)) }

        val transform = new DrillTransform(transforms.toArray)
        transform.setInput(sourceOp)
        transform
    }

    private def compilePureRow(tup: TupleConstructor): ((Sym => String) => Vector[String]) = (env: Sym => String) =>
      tup.tuple.map {
        case _ /> link => ???
        case s: Sym => s""""`${env(s)}`""""
      }
  }
  
  class CompileDrill(override val queryEnv: Map[Sym, LogicalOperator] = Map.empty)
    extends CompileRelational with DrillCompilerPart

}

trait QueryInterpreter extends BaseQueryInterpreter with Queries with BaseExprInterpreter {
  override def evalQuery(inputRow: Row, q: Query): Row = q match {
    case _/>Transform(gen: Generator, table, select) =>
      push(gen.symsVector.zip(inputRow.values.map(new Literal(_))))
      evalExpression(select).asInstanceOf[Row]
    case _ => super.evalQuery(inputRow, q)
  }

  override def evalExpression(expr: Expression): Any = expr match {
    case _/>Pure(e) => evalExpression(e)
    case _ => super.evalExpression(expr)
  }
}

trait TableCompilerPhases extends LazyLogging with Symbols with Queries with TableUtils {
  import org.kiama.rewriting.Rewriter._

  class LeafSyms(tree: GraphTree) extends Attribution { self =>
    val leafSyms: Expression => Set[Sym] = attr {
      case Expr(node) => tree.child(node).map(self.leafSyms).foldLeft(Set.empty[Sym]) {_ ++ _}
      case s: Sym => Set(s)
    }
  }

  object ClosestFilter extends Attribution {
    val closestFilter: Expression => Option[(Generator, Atom)] = attr {
      case _ /> Filter(gen: Generator, _, filter) => (gen, filter).some
      case _ /> Join(_, _, InnerJoin, GenPlusFilter(gen, filter)) => (gen, filter).some
      //      case _/>Project(_/> table, _) => table -> closestFilter
      case _ => None
    }
  }

  object OutputSchema extends Attribution {
    private val forTransform: Query => Vector[(String, TType)] = attr {
      case Transform(_, _, _/>Pure(_/>(t: TupleConstructor))) => t.names.zip(t.tuple.map(_.tpe))
    }

    val forComprehension: Comprehension => Option[Vector[(String, TType)]] = attr {
      case Comprehension(_/>(input: Comprehension), Nil, _, _, _, Nil) => this.forComprehension(input)
      case Comprehension(input, transforms, _, _, _, Nil) => this.forTransform(transforms.last).some
      case _ => None
    }
  }

//  def doAnalyze_(tree: Expression): ValidationNel[String, Unit] = tree match {
//    case Transform(Expr(TupleConstructor(in)), table, Expr(Pure(TupleConstructor(out)))) => //simple map comprehension - nothing to rewrite
//      //for each field, find out where it went and what transformations were applied
//      val ins = in.asInstanceOf[Vector[Sym]].toSet
//      val outs = out.map(expr => expr -> leafSyms)
//
//      val dropped = ins diff (outs.reduce(_ ++ _)) //TODO move this to analyze/rewrite phase
//      if (dropped.nonEmpty) {
//        logger.debug(s"${dropped.size} of projected fields not used")
//      }
//      ().successNel[String]
//    case _ => ().successNel[String]
//  }

  /**
   * Merges nested expressions into joins
   * TODO change argument to atom
   * @return
   */
  val fuseTransforms = repeat(oncetd(query[Expression](fuseTransformsImpl))) andThen (_.map(_.asInstanceOf[Expression]))
  private def fuseTransformsImpl : Expression ==> Unit = {
    //nested transformation whose result is a pure expression
    case Some(sym) /> Transform(gPar: Generator, Some(parSym) /> Query(parTable),
    _ /> Transform(gChild: Generator, Some(childSym) /> Query(childTable), _ /> (select@Pure(_)))) =>
      logger.debug("found candidates for a merge with pure output")

      //see if there is a filter that contains both parent and child syms
      //TODO split filters
      //TODO remove old filter from tree
      //returns merged generator for the right table and some filter
      val (gMerged, filter): (Generator, Option[(Generator, Atom)]) = ClosestFilter.closestFilter(childTable) map (filter => {
        val (gFilter, sel) = filter
        logger.debug(s"Found a filter on child table: $sel")

        val Generator(parSyms, _) = gPar
        //TODO cache attribution
        val filterSyms = new LeafSyms(new GraphTree(sel)).leafSyms(sel)

        if (filterSyms.intersect(parSyms.toSet).nonEmpty) {
          logger.debug("filter selector contains syms from both parent and child transformation")
          val mergedGen = Generator.merge(gChild, gFilter)
          (mergedGen, (gFilter, sel).some)
        } else {
          logger.debug("filter selector does not contain syms from both parent and child transformation")
          (gChild, None)
        }
      }) getOrElse((gChild, None))

      val ((update, rightTable), generator) = Generator.fuse(parTable, gPar, childTable, gMerged)
      val join = Join((gPar, parSym), (gChild.update(update), rightTable),
        if (filter.isEmpty) CartesianJoin else InnerJoin,
        filter.map { case (gen, f) => (gen.update(update), f)})

      sym.replaceWith(Transform(generator, join, select))
  }


  def buildComprehensions = {
    //all the complexity here comes from the fact, that for Comprehension subexpressions
    //the strategy should traverse its inner expression (closure), but not the source
    //expression. this could be simplified by modyfing Comprehension so that it does not
    //link to the subexpressions' sources

    def traverseInner(s: Strategy): Strategy = {
      rulefs[Expression] {
        case None /> (q @ InnerQuery(_)) =>
          assert(q.productArity == 3, "kiama congruence constraint")
          congruence(id, id, s)
        case _ => s
      }
    }

    def skipComprehension(s: Strategy): Strategy = {
      lazy val skip: Strategy =
      rulefs[Expression] {
        case (c: Comprehension) =>
          assert(c.productArity == 6, "kiama congruence constraint")
          val v = Vector(id, id, id, id, id, id)
          //congruence(s <+ one(skip), id, id, id, id, id) <+ congruence(id, one(traverseInner(s)), id, id, id, id) <+ ...
          v.indices.drop(1).foldLeft(congruence(s <+ one(skip), id, id, id, id, id)){(strategy, ix) =>
            strategy <+ congruence(v.updated(ix, one(traverseInner(s))):_*)
          }
        case q @ _ => s <+ one(skip)
      }
      skip
    }

    def buildComprehensionsImpl: Expression ==> Atom = {
      case Some(sym) /> _ if ComprehensionBuilder.comprehension(sym).nonEmpty =>
        ComprehensionBuilder.comprehension(sym).get
    }

    repeat(skipComprehension(rule[Expression](buildComprehensionsImpl))) andThen (_.map(_.asInstanceOf[Expression]))
  }
}