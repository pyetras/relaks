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


trait TableOps extends Symbols {
  type RowN[L <: HList] = Rep[Tup[L]]
  type Row[A] = Rep[Tup[A :: HNil]]
  type Row2[A, B] = Rep[Tup[A :: B :: HNil]]
  type Row3[A, B, C] = Rep[Tup[A :: B :: C :: HNil]]

  class ProjectedTableComprehensions[FieldsLen <: Nat](fieldsLength: Int, projection: Atom) extends Rep[Table] {

    override val tree: Expression = projection

    private def tupleGenerator[F <: HList](): (TupleConstructor, Rep[Tup[F]]) = {
      val tupleTree = TupleConstructor((0 until fieldsLength).map(_ => fresh).toVector) //todo types
      val generator = new Rep[Tup[F]] {
          override val tree: Expression = tupleTree //TODO type
        }
      (tupleTree, generator)
    }

    def flatMap[F <: HList](f: Rep[Tup[F]] => Rep[Table])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      val (tupleTree, generator) = tupleGenerator()
      val mapper = f(generator)
      new Rep[Table] {
        override val tree: Atom = Transform(tupleTree, projection, mapper.tree)(new UntypedTableType)
      }
    }

    def map[F <: HList, T <: HList](f: Rep[Tup[F]] => Rep[Tup[T]])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      flatMap((x:RowN[F]) => RowRep(f(x)))
    }

    def withFilter(f: Rep[Tup[Nothing]] => Rep[Boolean]) = {
      val (tupleTree, generator) = tupleGenerator()
      val cond = f(generator).tree
      cond match {
        case Literal(true) => this //remove empty filters immediately, most likely occurs
                                   // with the cast matcher _.isInstanceOf in for comprehensions
        case _ => filterHelper(tupleTree, cond)
      }
    }

    private def filterHelper(tupleTree: TupleConstructor, cond: Expression): ProjectedTableComprehensions[FieldsLen] = {
//      val _/>Project(original, fields) = projection
      //filtering does not cause projection
      val filteredProjection = Filter(tupleTree, projection, cond)
      new Rep[Table] {
        override val tree: Expression = filteredProjection
      }
      new ProjectedTableComprehensions[FieldsLen](fieldsLength, filteredProjection)
    }

    //TODO: a filter version that does not cause projection
    def filter[F <: HList](f: Rep[Tup[F]] => Rep[Boolean])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      val (tupleTree, generator) = tupleGenerator()
      val cond = f(generator).tree
      filterHelper(tupleTree, cond)
    }
  }
  
  class TableOperations(arg1: Rep[Table]) {
    def apply[P <: Product, FieldsLen <: Nat](fields: P)(implicit /*tupEv: IsTuple[P],*/
                                                      lenEv: tuple.Length.Aux[P, FieldsLen],
                                                      fieldsLength: ToInt[FieldsLen],
                                                      toVector: ToTraversable.Aux[P, Vector, Symbol]) = {
      
      val projection = Project(arg1.tree, toVector(fields))(new UntypedTableType)

      new ProjectedTableComprehensions[FieldsLen](fieldsLength(), projection)
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

trait TableComprehensionRewriter extends LazyLogging with Symbols {
  private def leafSyms: Expression => Set[Sym] = attr { tree =>
    tree match {
      case Expr(node) => node.children.map(c => c.asInstanceOf[Expression] -> leafSyms).foldLeft(Set.empty[Sym]) {_ ++ _}
      case s: Sym => Set(s)
    }
  }

  def doAnalyze_(tree: Expression): ValidationNel[String, Unit] = tree match {
    case Transform(Expr(TupleConstructor(in)), table, Expr(Pure(TupleConstructor(out)))) => //simple map comprehension - nothing to rewrite
      //for each field, find out where it went and what transformations were applied
      val ins = in.asInstanceOf[Vector[Sym]].toSet
      val outs = out.map(expr => expr -> leafSyms)

      val dropped = ins diff (outs.reduce(_ ++ _)) //TODO move this to analyze/rewrite phase
      if (dropped.nonEmpty) {
        logger.debug(s"${dropped.size} of projected fields not used")
      }
      ().successNel[String]
    case _ => ().successNel[String]
  }

  private def closestFilter: Expression => Option[Filter] = attr { tree =>
    tree match {
      case f: Filter => f.some
      case Project(_/> table, _) => table -> closestFilter
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
    case Transform(TupleConstructor(parSyms), parTable, _/> Transform(TupleConstructor(childSyms), childTable, _/> (select @ Pure(_)))) =>
      logger.debug("found candidates for a merge with pure output")
      //see if there is a filter that contains both parent and child syms
      val filter: Option[(TupleConstructor, TTree)] = (childTable -> closestFilter) flatMap { filter =>
        logger.debug(s"Found a filter on child table $filter")
        val Filter(generator, _, sel) = filter
        val filterSyms = sel -> leafSyms
        if (filterSyms.intersect(parSyms.asInstanceOf[Vector[Sym]].toSet).nonEmpty &&
          filterSyms.intersect(childSyms.asInstanceOf[Vector[Sym]].toSet).nonEmpty) {
          logger.debug("filter selector contains syms from both parent and child transformation")
          (generator, sel).some
        } else {
          logger.debug("filter selector does not contain syms from both parent and child transformation")
          None
        }
      }
      val generator = TupleConstructor(parSyms ++ childSyms)
      val join = Join(parTable, childTable, if (filter.isDefined) InnerJoin else CartesianJoin, filter)
      Transform(generator, join, select)
//    case Expr(nested @ Transform(_, _, Expr(Transform(_, _, _)))) => //nested comprehension
//      //collect all tables
//      def collect(t: Transform, acc: Vector[(Vector[Expression], TTree)] = Vector.empty): (Vector[(Vector[Expression], TTree)], TTree) =
//        t match {
//          case Transform(TupleConstructor(gen), table, _/> (next @ Transform(_, _, _))) => collect(next, acc :+ (gen, table))
//          case Transform(TupleConstructor(gen), table, _/> (select @ Pure(_))) => (acc :+ (gen, table), select)
//        }
//      val (gensTables, select) = collect(nested)
//      val generator = TupleConstructor(gensTables.map(_._1).reduce(_ ++ _))
//
//      val fst +: snd +: tables = gensTables.map(_._2)
//      val zero: Atom = Join(fst, snd, CartesianJoin, List.empty)
//      val join = tables.foldLeft(zero)((join, right) => Join(join, right, CartesianJoin, List.empty))
//      Transform(generator, join, select)
//
//    case _ => ???
  }
}