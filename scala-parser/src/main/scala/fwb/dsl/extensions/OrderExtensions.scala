package fwb.dsl.extensions

import fwb.dsl._
import AST._

import scala.language.implicitConversions
import scala.language.reflectiveCalls

/**
 * Created by Pietras on 16/04/15.
 */
trait OrderExtensions extends ASTSyntax with BoolExtensions {
  class OrderOperations[B1](val arg1: Rep[B1]) extends Operations[B1] {
    def <(arg: Rep[B1]): Rep[Boolean] =
      op.arg2[B1].to[Boolean].toRep(Stdlib.<, arg1.tree, arg.tree)
    def <=(arg: Rep[B1]): Rep[Boolean] =
      op.arg2[B1].to[Boolean].toRep(Stdlib.<=, arg1.tree, arg.tree)
    def >(arg: Rep[B1]): Rep[Boolean] =
      op.arg2[B1].to[Boolean].toRep(Stdlib.>, arg1.tree, arg.tree)
    def >=(arg: Rep[B1]): Rep[Boolean] =
      !(this < arg)

  }

  implicit def addOrderOps[B1](operand: Rep[B1])(implicit ev2: UnliftedArgType[B1], o: Ordering[B1]) =
    new OrderOperations[B1](operand)

}
