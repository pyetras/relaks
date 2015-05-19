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

trait TableIO extends LabelledTuples with Symbols with BaseRelationalCompiler {
  def load(path: String): Rep[Table] = new Rep[Table] {
    override val tree: Atom = LoadTableFromFs(path)(new UntypedTableType)
  }

  def load[S <: HList](path: String, schema: Rep[Tup[S]]): Rep[TypedTable[S]] = new Rep[TypedTable[S]] {
    override val tree: Atom = LoadTableFromFs(path)(schema.getTpe)
  }

  def store[T <: Table](table: Rep[T]): Unit = storedOutput += table.tree
}

trait BaseRelationalCompiler {
  val storedOutput: mutable.Set[Expression] = mutable.Set.empty
}
