package relaks.lang.dsl.extensions

import com.typesafe.scalalogging.LazyLogging
import org.kiama.attribution.Attribution.attr
import org.kiama.rewriting.Rewriter.rule
import org.kiama.rewriting.Strategy
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.AST.syntax._
import relaks.lang.dsl._
import relaks.lang.dsl.extensions.ast._
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.ops.tuple.ToTraversable
import shapeless.ops.{hlist, tuple}

import scala.language.{existentials, implicitConversions}
import scalaz.Scalaz._
import scalaz.ValidationNel

/**
 * Created by Pietras on 17/05/15.
 */
trait TableExtensions extends TableIO with TableOps {
  
}

trait TableUtils extends Symbols {
  class Generator(val symsToFields: Map[Sym, Symbol], val symsToTables: Map[Sym, Atom]) extends GeneratorBase {
    def this(syms_ : Vector[Sym], fields_ : Vector[Symbol], table_ : Atom) =
      this(syms_ zip fields_ toMap, syms_ zip (0 until syms_.length).map(_ => table_) toMap)


    def unifyWith(other: Generator): Generator = new Generator(symsToFields ++ other.symsToFields, symsToTables ++ other.symsToTables)
  }

  object Generator {
    def apply(syms: Vector[Sym], fields: Vector[Symbol], table: Atom) = new Generator(syms, fields, table)
    def unapply(generator: Generator): Option[(Iterable[Sym], Iterable[Symbol])] = generator.symsToFields.unzip.some
  }

  object GenPlusFilter {
    def unapply(opt: Option[(GeneratorBase, Atom)]) = for {
      (g: Generator, filter) <- opt
    } yield (g, filter)
  }
}

trait TableOps extends Symbols with TableUtils {

  type RowN[L <: HList] = Rep[Tup[L]]
  type Row[A] = Rep[Tup[A :: HNil]]
  type Row2[A, B] = Rep[Tup[A :: B :: HNil]]
  type Row3[A, B, C] = Rep[Tup[A :: B :: C :: HNil]]

  class ProjectedTableComprehensions[FieldsLen <: Nat](fieldsLength: Int, fields: Vector[Symbol], table: Atom) extends Rep[Table] {

    override val tree: Expression = table

    private def tupleGenerator[F <: HList](): (Generator, Rep[Tup[F]]) = {
      val syms = (0 until fieldsLength).map(_ => fresh).toVector
      val tupleTree = TupleConstructor(syms) //todo types
      val generator = new Rep[Tup[F]] {
          override val tree: Expression = tupleTree //TODO type
        }
      (Generator(syms, fields, table), generator)
    }

    def flatMap[F <: HList](f: Rep[Tup[F]] => Rep[Table])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      val (tupleTree, generator) = tupleGenerator()
      val mapper = f(generator)
      new Rep[Table] {
        override val tree: Atom = Transform(tupleTree, table, mapper.tree)(new UntypedTableType)
      }
    }

    def map[F <: HList, T <: HList](f: Rep[Tup[F]] => Rep[Tup[T]])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      flatMap((x:RowN[F]) => RowRep(f(x)))
    }

    def withFilter(f: Rep[Tup[Nothing]] => Rep[Boolean]) = {
      val (generator, rep) = tupleGenerator()
      val cond = f(rep).tree
      cond match {
        case Literal(true) => this //remove empty filters immediately, most likely occurs
                                   // with the cast matcher _.isInstanceOf in for comprehensions
        case _ => filterHelper(generator, cond)
      }
    }

    private def filterHelper(generator: Generator, cond: Expression): ProjectedTableComprehensions[FieldsLen] = {
      val filteredTable = Filter(generator, table, cond)
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

  def unnestTransforms: Strategy = rule[Expression] {
    //nested transformation whose result is a pure expression
    case Transform(gPar: Generator, parTable,
                  _/> Transform(gChild: Generator, childTable, _/> (select @ Pure(_)))) =>
      logger.debug("found candidates for a merge with pure output")

      //see if there is a filter that contains both parent and child syms
      val filter: Option[(Generator, Atom)] = (childTable -> closestFilter) flatMap { filter =>
        logger.debug(s"Found a filter on child table $filter")
        val (filterGen, sel) = filter
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