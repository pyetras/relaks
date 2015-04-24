package relaks.lang.dsl

import relaks.lang.dsl.AST.ASTSyntax
import relaks.lang.dsl.extensions._

import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.language.implicitConversions
import org.kiama.attribution.Attribution._

/**
 * Created by Pietras on 14/04/15.
 */
trait DSL
  extends ASTSyntax
  with Symbols
  with SuperPosExtensions
  with AnyExtensions
  with BoolExtensions
  with OrderExtensions
  with NumericExtensions
  with ListExtensions
  with ProductExtensions
  with ContCompiler {

  import AST._
  private var stored = new mutable.MutableList[Rep[Any]]

  def store[T](rep: Rep[T])(implicit ev: UnliftedArgType[T]) = {
    assert(!rep.getTpe.isSuperPosed, "cannot store a not determined value")
    stored += rep
  }
}
