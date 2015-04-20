package fwb.dsl

import fwb.dsl.AST.ASTSyntax
import fwb.dsl.extensions._

import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.language.implicitConversions

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
  with ProductExtensions {

  import AST._
  private var stored = new mutable.MutableList[Rep[Any]]

  @implicitNotFound("Cannot store a not determined value")
  def store[T](rep: Rep[T])(implicit ev: UnliftedArgType[T]) = stored += rep


}
