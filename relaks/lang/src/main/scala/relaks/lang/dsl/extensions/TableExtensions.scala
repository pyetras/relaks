package relaks.lang.dsl.extensions

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.core.JsonParser.Feature
import com.fasterxml.jackson.databind.{SerializationFeature, ObjectMapper}
import com.fasterxml.jackson.databind.module.SimpleModule
import com.typesafe.scalalogging.LazyLogging
import org.apache.calcite.rel.core.JoinRelType
import org.apache.drill.common.JSONOptions
import org.apache.drill.common.config.DrillConfig
import org.apache.drill.common.expression.{SchemaPath, LogicalExpression, FieldReference}
import org.apache.drill.common.logical.data.{Transform => DrillTransform, Join => DrillJoin, NamedExpression, JoinCondition, LogicalOperator, Scan}
import org.kiama.==>
import org.kiama.attribution.Attribution.attr
import org.kiama.rewriting.Rewriter.rule
import org.kiama.rewriting.{Rewriter, Strategy}
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.AST.syntax._
import relaks.lang.dsl._
import relaks.lang.dsl.extensions.ast._
import relaks.lang.dsl.utils.TreePrettyPrintable
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
  sealed trait ForTableQuery

  protected val getTable: Atom => Sym @@ ForTableQuery = attr {
    case Some(sym) /> (_: TableQuery) => Tag(sym)
    case StepTable(q) => q -> getTable
  }

  def forTable(sym: Sym): Sym @@ ForTableQuery = ForTable.unapply(sym).get

  object ForTable {
    def unapply(sym: Sym) = sym match {
      case Some(s) /> (_: TableQuery) => Tag[Sym, ForTableQuery](s).some
      case _ => None
    }
  }

}

trait TableOps extends Symbols with Queries {

  type RowN[L <: HList] = Rep[Tup[L]]
  type Row[A] = Rep[Tup[A :: HNil]]
  type Row2[A, B] = Rep[Tup[A :: B :: HNil]]
  type Row3[A, B, C] = Rep[Tup[A :: B :: C :: HNil]]

  class ProjectedTableComprehensions[FieldsLen <: Nat](fields: Vector[Symbol], query: Atom) extends Rep[Table] {

    override val tree: Expression = query

    private def tupleGenerator[F <: HList](): (Generator, Rep[Tup[F]]) = {
      val gen = Generator.fromFields(fields)
      val tuple = gen.toTuple[F] //todo types
      (gen, tuple)
    }

    def flatMap[F <: HList](f: Rep[Tup[F]] => Rep[Table])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      val (gen, tuple) = tupleGenerator()
      val mapper = f(tuple)
      new Rep[Table] {
        override val tree: Atom = Transform(gen, query, mapper.tree)(new UntypedTableType)
      }
    }

    def map[F <: HList, T <: HList](f: Rep[Tup[F]] => Rep[Tup[T]])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      flatMap((x:RowN[F]) => RowRep(f(x)))
    }

    //todo: withfilter and filter should not force a projection,
    // therefore additional class (TableMonadic, TableProjection and TableWithFilter) is required
    def withFilter(f: Rep[Tup[Nothing]] => Rep[Boolean]) = {
      val (generator, rep) = tupleGenerator()
      val cond = f(rep).tree
      cond match {
        case _/>Literal(true) => this //remove empty filters immediately, most likely occurs
                                   // with the cast matcher _.isInstanceOf in for comprehensions
        case _ => filterHelper(generator, cond)
      }
    }

    private def filterHelper(generator: Generator, cond: Expression): ProjectedTableComprehensions[FieldsLen] = {
      val filteredTable = Filter(generator, query, cond)
      new Rep[Table] {
        override val tree: Expression = filteredTable
      }
      new ProjectedTableComprehensions[FieldsLen](fields, filteredTable)
    }

    //TODO: a filter version that does not cause projection
    def filter[F <: HList](f: Rep[Tup[F]] => Rep[Boolean])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      val (generator, rep) = tupleGenerator()
      val cond = f(rep).tree
      filterHelper(generator, cond)
    }
  }
  
  class TableOperations(arg1: Rep[Table]) {
    def apply[P <: Product, FieldsLen <: Nat](fields: P)(implicit /*tupEv: IsTuple[P],*/
                                                      lenEv: tuple.Length.Aux[P, FieldsLen],
                                                      fieldsLength: ToInt[FieldsLen],
                                                      toVector: ToTraversable.Aux[P, Vector, Symbol]) = {

      new ProjectedTableComprehensions[FieldsLen](toVector(fields), arg1.tree)
    }
  }

  implicit def addTableOps(t: Rep[Table]): TableOperations = new TableOperations(t)
}

trait TableIO extends Symbols with BaseRelationalCompiler {
  def load(path: String): Rep[Table] = new Rep[Table] {
    override val tree: Atom = LoadTableFromFs(path)(new UntypedTableType)
  }

  def load[S <: HList](path: String, schema: Rep[Tup[S]]): Rep[TypedTable[S]] = new Rep[TypedTable[S]] {
    override val tree: Atom = LoadTableFromFs(path)(schema.getTpe)
  }

  def store[T <: Table](table: Rep[T]): Unit = storedOutput += table.tree
}

trait BaseRelationalCompiler {
  var storedOutput: Set[Expression] = Set.empty
}

trait DrillCompiler extends BaseRelationalCompiler with Symbols with Queries with LazyLogging with TableUtils {
  type Duplicate = Map[Symbol, Vector[Sym]]
  type Renamer = Map[(Sym @@ ForTableQuery, Symbol), Symbol]

  def collectDuplicates: (Vector[Duplicate], Atom) ==> Writer[Map[Sym @@ ForTableQuery, Duplicate], Unit] = {
    case (duplicates, Some(sym) /> (table: TableQuery)) => {

      //unpack next steps and optionally include filter duplicates for join query
      val (dups, sources): (Vector[Duplicate], Seq[(Vector[Duplicate], Atom)])= table match {
        case Join((lgen: Generator, left), (rgen: Generator, right), _, cond) =>
          (duplicates ++ cond.map(_._1.asInstanceOf[Generator].duplicates).toVector,
            Seq((Vector(lgen.duplicates), left), (Vector(rgen.duplicates), right)))

        case QueryWithGenerator(gen, StepTable(next)) => (duplicates, Seq((Vector(gen.duplicates), next)))

        case StepTable(next) => (duplicates, Seq((Vector.empty[Duplicate], next)))

        case _ => (duplicates, Seq.empty)
      }

      if (dups.exists(_.nonEmpty)) logger.debug(s"collecting $dups")

      //get biggest name conflict for each symbol
      //might break, as multiple syms can belong to one generator, it should be a max
      //of a generator mapping - actually right now should never be greater > 2 so no problem lol
      implicit val vecOrder: scala.Ordering[Vector[Sym]] = new scala.Ordering[Vector[Sym]] {
        override def compare(x: Vector[Sym], y: Vector[Sym]): Int = x.length compare y.length
      }
      val m = dups.map(_.mapValues(iter => Vector(iter)))
                  .foldLeft1Opt(_ |+| _).map(_.mapValues(_.max))
                  .filter(_.nonEmpty)
                  .map(dup => Map((forTable(sym), dup))).getOrElse(Map.empty[Sym @@ ForTableQuery, Duplicate])

      sources.foldLeft(m.tell)((writer, genSource) => writer flatMap (_ => collectDuplicates(genSource)))
    }
    case (duplicates, QueryWithGenerator(gen, StepTable(next))) => collectDuplicates((duplicates :+ gen.duplicates, next))
  }

  class CompileDrill(duplicates: Map[Sym @@ ForTableQuery, Duplicate], val queryEnv: Map[Sym, LogicalOperator] = Map.empty) {
    var fieldsEnv: Renamer = Map.empty
    var emittedQueries: Map[Sym, LogicalOperator] = Map.empty

    private def fromCache(sym: Sym) = queryEnv.get(sym).orElse(emittedQueries.get(sym))

    def apply(root: Expression): Writer[Map[Sym, LogicalOperator], LogicalOperator] = {
      val result = compile(root)
      Writer(emittedQueries, result)
    }

//    private def duplicateFields: Seq[()]

    protected def compile(expr: Expression): LogicalOperator = expr match {
      case Some(sym) /> _ if fromCache(sym).isDefined =>
        val op = fromCache(sym).get
        emittedQueries += ((sym, op))
        op
      case Some(sym) /> Query(q) =>
//        materializeProjections.applyOrElse(expr, (_: Expression) => ()) // do this in a separate phase
        val op = compileQuery(q)
        emittedQueries += ((sym, op))
        op
    }

    def materializeProjections: Expression ==> Unit = {
      case Some(joinSym@ForTable(sym)) /> (join@Join((gleft: Generator, left), (gright: Generator, right), _, _)) if duplicates.contains(sym) =>
        //group by generator each sym belongs to
        //there are only two generators
        //each of them must have at least one sym from duplicates
        //each sym must belong to either one or the other
        val dups = duplicates(sym)
        dups.mapValues(syms => Seq(gleft, gright).filter(gen => syms.any(sym => gen.contains(sym))))
          .mapValues(gens => if (gens.length != 2) {
          logger.error(s"duplicate syms belong just to $gens, while they should be in ($gleft, $gright)")
          throw new IllegalArgumentException
        })

        val extra = dups.mapValues(syms => syms.all(sym => Seq(gleft, gright).exists(_.contains(sym)))).find(kv => !kv._2)
        if (extra.isDefined) {
          logger.error(s"got field ${extra.get._1} that doesn't belong to any source generator")
          throw new IllegalArgumentException
        }

        //select all but one subquery
        //always select right
        val _ /> Query(subTable) = right

        def rename(field: Symbol, sym: Sym): Symbol = {
          //TODO something safer
          Symbol(s"${field.name}___${sym.name}")
        }
        val fields = dups.toVector.map(kv => (kv._1, rename(kv._1, kv._2.head)))
        fields.foreach { case (old, n) => fieldsEnv += (((sym, old), n)) }
        //apply projections to those selections
        val projection = Project(subTable, fields)

        //update syms
        //its dangerous to update something else than self (maps are indexed by syms)
        //        subSym.replaceWith(projection)
        val newJoin = join.copy(right = (gright, projection))
        joinSym.replaceWith(newJoin) //see symbols -> sym constructor
    }

    lazy val mapper = {
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

    def compileQuery(q: Query): LogicalOperator = q match {
      case LoadTableFromFs(path) =>
        val optionsJson = "{\"format\" : {\"type\" : \"parquet\"},\"files\" : [ \"file:"  + path + "\" ]}"
        val opts = mapper.readValue(optionsJson, classOf[JSONOptions])
        val scan = new Scan("dfs", opts)
        scan
      case Join((_, left), (_, right), typ, conditions) =>
        val leftOp = compile(left)
        val rightOp = compile(right)

        val joinT = typ match {
          case CartesianJoin => JoinRelType.INNER
          case InnerJoin => JoinRelType.INNER
        }

        new DrillJoin(leftOp, rightOp, Array.empty[JoinCondition], joinT)

      case Transform(gen: Generator, source, _/>Pure(_/>(select: TupleConstructor))) =>
        val sourceOp = compile(source)
        val table = source -> getTable

        val env = (sym: Sym) => fieldsEnv.getOrElse((table, gen.symsToFields(sym)), gen.symsToFields(sym)).name
        val transforms = compilePureRow(select)(env).zip(select.names).map
          { case (expr, name) => new NamedExpression(mapper.readValue(expr, classOf[LogicalExpression]), new FieldReference(name)) }

        val transform = new DrillTransform(transforms.toArray)
        transform.setInput(sourceOp)
        transform
    }

    def compilePureRow(tup: TupleConstructor): ((Sym => String) => Vector[String]) = (env: Sym => String) =>
      tup.tuple.map {
        case _ /> link => ???
        case s: Sym => s""""`${env(s)}`""""
      }

  }

//  def compileLogical(expr: Atom) =
//    expr match {
//      case Some(sym) /> _ if symtab.contains(sym) => (symtab, symtable => symtable(sym))
//      case Some(sym) /> (q: Query) =>
//        compileQuery(q).flatMap(nodef => {
//          val (node, f) = nodef
//          SymState (newsymtab => (newsymtab + (sym -> node), f))
//        }).apply(symtab)
//    }
//  }

}

trait TableComprehensionRewriter extends LazyLogging with Symbols with Queries {
  private def leafSyms: Expression => Set[Sym] = attr { tree =>
    tree match {
      case Expr(node) => node.children.map(c => c.asInstanceOf[Expression] -> leafSyms).foldLeft(Set.empty[Sym]) {_ ++ _}
      case s: Sym => Set(s)
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

  private def closestFilter: Expression => Option[(Generator, Atom)] = attr { tree =>
    tree match {
      case _/> Filter(gen: Generator, _, filter) => (gen, filter).some
      case _/> Join(_, _, InnerJoin, GenPlusFilter(gen, filter)) => (gen, filter).some
//      case _/>Project(_/> table, _) => table -> closestFilter
      case _ => None
    }
  }

  private def tableSource: Expression => Option[Expression] = attr { tree =>
    tree match {
      case t: LoadTableFromFs => t.some
      case t: Join => t.some
      case t: Limit => t.some
      case t: GroupBy => t.some
      case Transform(_, _/> table, _) => table -> tableSource
      case Filter(_, _/> table, _) => table -> tableSource
    }
  }

  /**
   * Merges nested expressions into joins
   * TODO change argument to atom
   * @return
   */
  def unnestTransforms: Expression ==> Unit = {
    //nested transformation whose result is a pure expression
    case Some(sym) /> Transform(gPar: Generator, parTable,
    _ /> Transform(gChild: Generator, childTable, _ /> (select@Pure(_)))) =>
      logger.debug("found candidates for a merge with pure output")

      //see if there is a filter that contains both parent and child syms
      //TODO split filters
      //TODO remove old filter from tree
      val filter: Option[(Generator, Atom)] = (childTable -> closestFilter) flatMap { filter =>
        val (filterGen, sel) = filter
        logger.debug(s"Found a filter on child table: $sel")
        val filterSyms = sel -> leafSyms
        val Generator(parSyms, _) = gPar

        if (filterSyms.intersect(parSyms.toSet).nonEmpty) {
          logger.debug("filter selector contains syms from both parent and child transformation")
          val mergedGen = gPar.unifyWith(filterGen)
          (mergedGen, sel).some
        } else {
          logger.debug("filter selector does not contain syms from both parent and child transformation")
          None
        }
      }

      val generator = gPar.unifyWith(gChild)
      val join = Join((gPar, parTable), (gChild, childTable), if (filter.isEmpty) CartesianJoin else InnerJoin, filter)

      sym.replaceWith(Transform(generator, join, select))
  }

}