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
import org.kiama.rewriting.Rewriter.{query, manybu, repeat, oncetd}
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

  protected type Duplicate = Map[Symbol, Vector[Sym]]
  protected type Renamer = Map[Sym @@ ForTableQuery, Map[Symbol, Symbol]]
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

trait DrillCompilers extends BaseRelationalCompiler with Symbols with Queries with LazyLogging with TableUtils {
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

  sealed trait DrillCompilerBase {
    protected val fieldsEnv: Renamer
    protected var emittedQueries: Map[Sym, LogicalOperator] = Map.empty
    protected val queryEnv: Map[Sym, LogicalOperator]
  }
  
  trait DrillCompilerPart extends DrillCompilerBase {
    private def fromCache(sym: Sym) = queryEnv.get(sym).orElse(emittedQueries.get(sym))

    def compileDrill(expr: Expression): LogicalOperator = expr match {
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

    private def fieldName(table: Sym @@ ForTableQuery, trueName: Sym => Symbol)(sym: Sym) = {
      fieldsEnv.get(table).flatMap(_.get(trueName(sym))).getOrElse(trueName(sym)).name
    }

    def compileQuery(q: Query): LogicalOperator = q match {
      case LoadTableFromFs(path) =>
        val optionsJson = "{\"format\" : {\"type\" : \"parquet\"},\"files\" : [ \"file:"  + path + "\" ]}"
        val opts = mapper.readValue(optionsJson, classOf[JSONOptions])
        val scan = new Scan("dfs", opts)
        scan

      case Join((_, left), (_, right), typ, conditions) =>
        val leftOp = compileDrill(left)
        val rightOp = compileDrill(right)

        val joinT = typ match {
          case CartesianJoin => JoinRelType.INNER
          case InnerJoin => JoinRelType.INNER
        }

        new DrillJoin(leftOp, rightOp, Array.empty[JoinCondition], joinT)

      case Transform(gen: Generator, source, _/>Pure(_/>(select: TupleConstructor))) =>
        val sourceOp = compileDrill(source)
        val table = source -> getTable

        val env = fieldName(table, gen.symsToFields) _
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

  class CompileDrill(override val fieldsEnv: Renamer, override val queryEnv: Map[Sym, LogicalOperator] = Map.empty) extends DrillCompilerPart {
    def apply(root: Expression): Writer[Map[Sym, LogicalOperator], LogicalOperator] = {
      val result = compileDrill(root)
      Writer(emittedQueries, result)
    }
  }

}

trait TableCompilerPhases extends LazyLogging with Symbols with Queries with TableUtils {
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

  def collectDuplicates: (Atom) ==> Writer[Map[Sym @@ ForTableQuery, Duplicate], Unit] = {

    def collectDuplicatesHlp: (Vector[Duplicate], Atom) ==> Writer[Map[Sym @@ ForTableQuery, Duplicate], Unit] = {
      case (duplicates, Some(sym) /> (table: TableQuery)) => {

        //unpack next steps and optionally include filter duplicates for join query
        val (dups, sources): (Vector[Duplicate], Seq[(Vector[Duplicate], Atom)]) = table match {
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

        sources.foldLeft(m.tell)((writer, genSource) => writer flatMap (_ => collectDuplicatesHlp(genSource)))
      }
      case (duplicates, QueryWithGenerator(gen, StepTable(next))) => collectDuplicatesHlp((duplicates :+ gen.duplicates, next))
    }

    { case a: Atom if collectDuplicatesHlp.isDefinedAt((Vector.empty, a)) => collectDuplicatesHlp((Vector.empty, a)) }
  }

  //Map[(Sym @@ ForTableQuery, Symbol), Symbol]
  def renameFields(duplicates: Map[Sym @@ ForTableQuery, Duplicate]): Renamer = {
    duplicates.flatMap {
      case (sym, dups) =>
        def rename(field: Symbol, sym: Sym): Symbol = {
          //TODO something safer
          Symbol(s"${field.name}___${sym.name}")
        }
        val fields = dups.toVector.map(kv => (kv._1, rename(kv._1, kv._2.head)))
        Map((sym, fields.toMap))
    }
  }

  def validateDuplicates(duplicates: Map[Sym @@ ForTableQuery, Duplicate]): ValidationNel[String, Unit] = {
    duplicates.foldLeft(().successNel[String])((acc, symDups) => (Tag.unwrap(symDups._1), symDups._2) match {
      case (Some(ForTable(sym)) /> (join@Join((gleft: Generator, _), (gright: Generator, _), _, _)), dups) =>
      //group by generator each sym belongs to
      //there are only two generators
      //each of them must have at least one sym from duplicates
      val dups = duplicates(sym)
      val containsFromBothValidation = dups.mapValues(syms => Seq(gleft, gright).filter(gen => syms.any(sym => gen.contains(sym))))
        .find(kgens => kgens._2.length != 2)
        .map(kgens => s"duplicate syms belong just to ${kgens._2}, while they should be in ($gleft, $gright)".failureNel[Unit])
        .getOrElse(().successNel[String])

      //each sym must belong to either one or the other
      val extra = dups.mapValues(syms => syms.all(sym => Seq(gleft, gright).exists(_.contains(sym)))).find(kv => !kv._2)
      val extraSymsValidation = extra.map(e => s"got field ${e._1} that doesn't belong to any source generator".failureNel)
                                     .getOrElse(().successNel)

      acc *> containsFromBothValidation *> extraSymsValidation
    })
  }

  //TODO: make this safe to apply repeatedly
  def materializeProjections(fieldsEnv: Renamer) =
    manybu(query[Expression](materializeProjections_(fieldsEnv))) andThen (_.map(_.asInstanceOf[Expression]))
  private def materializeProjections_(fieldsEnv: Renamer): Expression ==> Unit = {
    case Some(joinSym@ForTable(sym)) /> (join@Join(_, (gright: Generator, right), _, _)) if fieldsEnv.contains(sym) =>
      //select all but one subquery
      //always select right
      val _ /> Query(subTable) = right

      def rename(field: Symbol, sym: Sym): Symbol = {
        //TODO something safer
        Symbol(s"${field.name}___${sym.name}")
      }
      val fields = fieldsEnv(sym).toVector

      //apply projections to those selections
      val projection = Project(subTable, fields)

      //update syms
      //its dangerous to update something else than self (maps are indexed by syms)
      //        subSym.replaceWith(projection)
      val newJoin = join.copy(right = (gright, projection))
      joinSym.replaceWith(newJoin) //see symbols -> sym constructor
  }


  /**
   * Merges nested expressions into joins
   * TODO change argument to atom
   * @return
   */
  val fuseTransforms = repeat(oncetd(query[Expression](unnestTransforms_))) andThen (_.map(_.asInstanceOf[Expression]))
  private def unnestTransforms_ : Expression ==> Unit = {
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

  private def forTable(sym: Sym): Sym @@ ForTableQuery = ForTable.unapply(sym).get

  private object ForTable {
    def unapply(sym: Sym) = sym match {
      case Some(s) /> (_: TableQuery) => Tag[Sym, ForTableQuery](s).some
      case _ => None
    }
  }


}