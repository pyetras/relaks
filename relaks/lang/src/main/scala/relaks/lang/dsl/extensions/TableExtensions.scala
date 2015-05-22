package relaks.lang.dsl.extensions

import com.typesafe.scalalogging.LazyLogging
import relaks.lang.dsl._
import AST._
import AST.syntax._
import shapeless._
import relaks.data.LabelledTuples
import shapeless.ops.nat.ToInt
import shapeless.ops.tuple.ToTraversable
import shapeless.ops.{hlist, tuple}
import org.kiama.attribution.Attribution.attr
import scalaz.{ValidationNel, Scalaz}
import Scalaz._

import scala.collection.mutable
import scala.language.implicitConversions

/**
 * Created by Pietras on 17/05/15.
 */
trait TableExtensions extends TableIO with TableOps {
  
}

trait QueryTrees extends Symbols {
  type Fields = Vector[Symbol]

  sealed trait TableQuery extends Query {
    override def tpe: AST.TType = new UntypedTableType
  }

  sealed case class Project(table: TTree, fields: Fields) extends TableQuery
  sealed case class Transform(generator: TupleConstructor, table: TTree, select: TTree) extends TableQuery

  sealed case class Join(left: TTree, right: TTree, typ: JoinType, conditions: List[(TTree, TTree)]) extends TableQuery
  trait JoinType
  object CartesianJoin extends JoinType
  object InnerJoin extends JoinType

  sealed case class Limit(table: TTree, start: TTree, count: TTree) extends TableQuery
  sealed case class Filter(generator: TupleConstructor, table: TTree, filter: TTree) extends TableQuery
  sealed case class GroupBy(generator: TupleConstructor, table: TTree, group: TTree) extends TableQuery
  sealed case class Pure(value: TTree) extends TableQuery
}

trait TableOps extends QueryTrees {
  type Row[A] = Rep[Tup[A :: HNil]]
  type Row2[A, B] = Rep[Tup[A :: B :: HNil]]
  type Row3[A, B, C] = Rep[Tup[A :: B :: C :: HNil]]

  class ProjectedTableComprehensions[FieldsLen <: Nat](fieldsLength: Int, projection: Atom) {
    def flatMap[F <: HList](f: Rep[Tup[F]] => Rep[Table])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      val tupleTree = TupleConstructor((0 until fieldsLength).map(_ => fresh).toVector) //todo types
      val generator = new Rep[Tup[F]] {
          override val tree: Expression = tupleTree //TODO type
        }

      val mapper = f(generator)
      new Rep[Table] {
        override val tree: Atom = Transform(tupleTree, projection, mapper.tree)(new UntypedTableType)
      }
    }

    def map[F <: HList, T <: HList](f: Rep[Tup[F]] => Rep[Tup[T]])(implicit lenEv: hlist.Length.Aux[F, FieldsLen]) = {
      flatMap((x:Rep[Tup[F]]) => RowRep(f(x)))
    }

    def withFilter(t: Nothing => Boolean) = {
      println(t)
      this
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

trait TableComprehensionRewriter extends QueryTrees with LazyLogging with Symbols {
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

  def unnestTransforms(expr: Expression): Expression = expr match {
    case t @ Expr(Transform(TupleConstructor(in), table, Expr(Pure(TupleConstructor(out))))) => //simple map comprehension - nothing to rewrite
      t
    case Expr(nested @ Transform(_, _, Expr(Transform(_, _, _)))) => //nested comprehension
      //collect all tables
      def collect(t: Transform, acc: Vector[(Vector[Expression], TTree)] = Vector.empty): (Vector[(Vector[Expression], TTree)], TTree) =
        t match {
          case Transform(TupleConstructor(gen), table, Expr(next @ Transform(_, _, _))) => collect(next, acc :+ (gen, table))
          case Transform(TupleConstructor(gen), table, select) => (acc :+ (gen, table), select)
        }
      val (gensTables, select) = collect(nested)
      val generator = TupleConstructor(gensTables.map(_._1).reduce(_ ++ _))

      val fst +: snd +: tables = gensTables.map(_._2)
      val zero: Atom = Join(fst, snd, CartesianJoin, List.empty)
      val join = tables.foldLeft(zero)((join, right) => Join(join, right, CartesianJoin, List.empty))
      Transform(generator, join, select)

    case _ => ???
  }
}