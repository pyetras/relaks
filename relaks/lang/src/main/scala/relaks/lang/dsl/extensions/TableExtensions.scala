package relaks.lang.dsl.extensions

import relaks.lang.dsl._
import AST._
import AST.syntax._
import shapeless._
import relaks.data.LabelledTuples

import scala.collection.mutable
import scala.language.implicitConversions
/**
 * Created by Pietras on 17/05/15.
 */
trait TableExtensions extends TableIO {

}

trait QueryTrees extends Symbols {
  type Fields = Vector[Symbol]

  sealed case class Project(table: TTree, fields: Fields) extends Query
  sealed case class Map(generator: Sym, table: TTree, select: TTree) extends Query
  sealed case class Take(table: TTree, count: TTree) extends Query
  sealed case class Filter(generator: Sym, table: TTree, filter: TTree) extends Query
  sealed case class GroupBy(generator: Sym, table: TTree, group: TTree) extends Query
  sealed case class Row(value: TTree) extends Query
}

trait TableOps extends QueryTrees {
  class TableOperations(arg1: Rep[Table]) {
    def apply(fields: Fields) = {
      object TableComprehensions {
        def flatMap[F <: HList, LU](f: Rep[Tup[F]] => Rep[Table]) = {
          val generator = freshRep[Tup[F]](UnknownType) //ugly
          val mapper = f(generator)
          new Rep[Table] {
            override val tree: Atom = Map(generator.tree.asInstanceOf[Sym], Project(arg1.tree, fields), mapper.tree)(new UntypedTableType) //Todo superpos
          }
        }

        def map[F <: HList, T <: HList](f: Rep[Tup[F]] => Rep[Tup[T]]) = {
          flatMap((x:Rep[Tup[F]]) => RowRep(f(x)))
        }

      }
      TableComprehensions
    }
  }

  private object RowRep {
    def apply[T <: HList](t: Rep[Tup[T]]) = new Rep[Table] {
      override val tree: Expression = Row(t.tree)(new UntypedTableType)
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
