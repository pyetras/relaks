package relaks.lang.dsl.extensions

import com.fasterxml.jackson.databind.ObjectMapper
import com.typesafe.scalalogging.LazyLogging
import org.apache.drill.common.JSONOptions
import org.apache.drill.common.logical.data.{LogicalOperator, Scan, Transform => DrillTransform}
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

trait TableUtils extends Symbols {
  sealed trait ForTableQuery

  type SymTables = Map[Sym, Set[Atom]]

  class Generator(val symsToFields: Map[Sym, Symbol], val symsToTables: Map[Sym, Sym @@ ForTableQuery]) extends GeneratorBase {
    def this(syms_ : Vector[Sym], fields_ : Vector[Symbol], table_ : Sym @@ ForTableQuery) =
      this(
        (syms_ zip fields_).toMap,
        (syms_ zip (0 until syms_.length).map(_ => table_)).toMap
      )
    
    lazy val fieldsTables: Map[Symbol, Set[Sym @@ ForTableQuery]] = symsToFields.foldLeft(Map.empty[Symbol, Set[Sym @@ ForTableQuery]]) { (map, symField) =>
      val (sym, field) = symField
      map |+| Map(field -> Set(symsToTables(sym)))
    }

    def hasDuplicateNames = fieldsTables.any(_.size > 1)

    def unifyWith(other: Generator): Generator = new Generator(symsToFields ++ other.symsToFields, symsToTables ++ other.symsToTables)

    /**
     * returns two generators, first contains only projections for table sym, second
     * just projections for tables != sym
     *
     * @param sym
     * @return
     */
    def splitOn(sym: Sym @@ ForTableQuery): (Generator, Generator) = {
      val newSymsToTables = symsToTables.span(_._2 == sym)
      val newSymsToFields = symsToFields.span(kv => newSymsToTables._1.contains(kv._1))
      (new Generator(newSymsToFields._1, newSymsToTables._1), new Generator(newSymsToFields._2, newSymsToTables._2))
    }

    override def toString: String = s"Generator[${symsToTables.groupBy(_._2).mapValues(_.keys) // Map[table -> sym]
                                                  .map({case (table, syms) => s"($table: ${syms.map(s => symsToFields(s)).mkString(", ")})"})
                                                  .mkString(", ")}]"
  }

  implicit val generatorSemigroup: Semigroup[Generator] = new Semigroup[Generator] {
    override def append(f1: Generator, f2: => Generator): Generator = f1.unifyWith(f2)
  }

  object Generator {
    def apply(syms: Vector[Sym], fields: Vector[Symbol], table: Sym @@ ForTableQuery) = new Generator(syms, fields, table)
    def unapply(generator: Generator): Option[(Iterable[Sym], Iterable[Symbol])] = generator.symsToFields.unzip.some
  }

  object GenPlusFilter {
    def unapply(opt: Option[(GeneratorBase, Atom)]) = for {
      (g: Generator, filter) <- opt
    } yield (g, filter)
  }

  object Query {
    def unapply(expr: Expression): Option[Query] = expr match {
      case _/>(q : Query) => q.some
      case _ => None
    }
  }

  object StepTable {
    def unapply(expr: Expression): Option[Atom] = expr match {
      case _/> Query(q) => q.stepTable
      case _ => None
    }
  }

  object Sources {
    def unapply(expr: Expression): Option[Seq[Atom]] = expr match {
      case _/> Query(q) => q.sources.some
      case _ => Seq.empty[Atom].some
    }
  }

  object QueryWithGenerator {
    def unapply(expr: Atom): Option[(Generator, Atom)] = expr match {
      case t @ (_/> Transform(g: Generator, _, _)) => (g, t).some
      case f @ (_/> Filter(g: Generator, _, _)) => (g, f).some
      case group @ (_/> GroupBy(g: Generator, _, _)) => (g, group).some
      case _ => None
    }
  }

  class QueryPrettyPrintable(q: Query) extends TreePrettyPrintable {
    override def printVerbose: Rdr = {
      val printSubs: Rdr =
        q.sources.foldLeft(noop)((rdr, source) => rdr.flatMap(_ => source match {
          case _ /> Query(sourceq) => sourceq.printVerbose
        }))

      for {
        _ <- println(q.mainToString)
        _ <- indent {
          printSubs
        }
      } yield ()
    }
  }

  implicit def exprPrettyPrintable(expr: Expression): TreePrettyPrintable = expr match {
    case _/>Query(q) => new QueryPrettyPrintable(q)
    case _ => new TreePrettyPrintable {
      override def printVerbose: Rdr = Reader(x => x._1 ++= expr.toString)
    }
  }

  protected val getTable: Atom => Sym @@ ForTableQuery = attr {
    case Some(sym) /> (_: TableQuery) => Tag(sym)
    case StepTable(q) => q -> getTable
  }

}

trait TableOps extends Symbols with TableUtils {

  type RowN[L <: HList] = Rep[Tup[L]]
  type Row[A] = Rep[Tup[A :: HNil]]
  type Row2[A, B] = Rep[Tup[A :: B :: HNil]]
  type Row3[A, B, C] = Rep[Tup[A :: B :: C :: HNil]]

  class ProjectedTableComprehensions[FieldsLen <: Nat](fieldsLength: Int, fields: Vector[Symbol], query: Atom) extends Rep[Table] {

    override val tree: Expression = query

    private def tupleGenerator[F <: HList](): (Generator, Rep[Tup[F]]) = {
      val syms = (0 until fieldsLength).map(_ => fresh).toVector
      val tupleTree = TupleConstructor(syms) //todo types
      val generator = new Rep[Tup[F]] {
          override val tree: Expression = tupleTree //TODO type
        }
      (Generator(syms, fields, query -> getTable), generator)
    }

    def flatMap[F <: HList](f: Rep[Tup[F]] => Rep[Table])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      val (tupleTree, generator) = tupleGenerator()
      val mapper = f(generator)
      new Rep[Table] {
        override val tree: Atom = Transform(tupleTree, query, mapper.tree)(new UntypedTableType)
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
      new ProjectedTableComprehensions[FieldsLen](fieldsLength, fields, filteredTable)
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

      new ProjectedTableComprehensions[FieldsLen](fieldsLength(), toVector(fields), arg1.tree)
    }
  }

  private object RowRep {
    def apply[T <: HList](t: Rep[Tup[T]]) = new Rep[Table] {
      override val tree: Expression = Pure(t.tree)(new UntypedTableType)
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

trait DrillCompiler extends BaseRelationalCompiler with Symbols with TableUtils with LazyLogging {
  def collectProjections: (Vector[Generator], Atom) ==> Writer[Map[Sym, Generator], Unit] = {
    case (generators, Some(sym) /> (table: TableQuery)) => {
      logger.debug(s"collecting $generators")
      val tqSym = sym -> getTable //should be == sym

      //unify generators and select only relevant
      val (m, otherGen) = generators.foldLeft1Opt(_ |+| _).map(gen => {
        val (myGenerator, otherGenerator) = gen.splitOn(tqSym)
        (Map((sym, myGenerator)), Vector(otherGenerator))
      }).getOrElse((Map.empty[Sym, Generator], Vector.empty[Generator]))

      val Sources(sources) = table
      sources.foldLeft(m.tell)((writer, source) => writer flatMap (_ => collectProjections((otherGen, source))))
    }
    case (generators, QueryWithGenerator(gen, StepTable(next))) => collectProjections((generators :+ gen, next))
  }

  class CompileDrill(projections: Map[Sym, Set[Generator]], var queryEnv: Map[Sym, LogicalOperator] = Map.empty) {
    var fieldsEnv = Map.empty[Sym, Symbol]
    def apply(root: Expression) = ???

//    private def duplicateFields: Seq[()]

    protected def compile(expr: Expression): LogicalOperator = expr match {
      case Some(sym) /> _ if queryEnv.contains(sym) => queryEnv(sym)
      case Some(sym) /> Query(q) =>
        val op = compileQuery(q)

//        projections(sym).si

        queryEnv += ((sym, op))
        op
    }

    def compileQuery(expr: Query): LogicalOperator = expr match {
      case LoadTableFromFs(path) =>
        val json = "{\"format\" : {\"type\" : \"parquet\"},\"files\" : [ \"file:/tmp/nation\" ]}"
        val mapper = new ObjectMapper()
        val opts = mapper.readValue(json, classOf[JSONOptions])
        val scan = new Scan("dfs", opts)
        scan
//      case t @ Transform(gen: Generator, table, select) =>
//        val parent = compile(table)
//        val transform = new DrillTransform()
//
//        transform.setInput(parent)
//
//        drillTable

    }

    def compileExpression(expr: Expression): String = ???
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

trait TableComprehensionRewriter extends LazyLogging with Symbols with TableUtils {
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
   * @return
   */
  def unnestTransforms: Expression ==> Atom = {
    //nested transformation whose result is a pure expression
    case _ /> Transform(gPar: Generator, parTable,
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
      val join = Join(parTable, childTable, if (filter.isEmpty) CartesianJoin else InnerJoin, filter)

      Transform(generator, join, select)
  }

}