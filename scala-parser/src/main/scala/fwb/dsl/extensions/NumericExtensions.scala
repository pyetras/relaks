package fwb.dsl.extensions

import fwb.dsl._
import AST._

import scala.language.implicitConversions

/**
 * Created by Pietras on 16/04/15.
 */
trait NumericExtensions extends ASTSyntax with Symbols {
  class NumericOperations[B1, P1](val arg1: Rep[P1]) extends Operations[B1, P1] {
    def + [P2, PR](arg2: Rep[P2])(implicit o: or#arg2[B1, P2]#to[B1, PR]) =
      o.toRep(Stdlib.+, arg1.tree, arg2.tree)

    def - [P2, PR](arg2: Rep[P2])(implicit o: or#arg2[B1, P2]#to[B1, PR]) =
      o.toRep(Stdlib.-, arg1.tree, arg2.tree)

    def * [P2, PR](arg2: Rep[P2])(implicit o: or#arg2[B1, P2]#to[B1, PR]) =
      o.toRep(Stdlib.*, arg1.tree, arg2.tree)

    def / [P2, PR](arg2: Rep[P2])(implicit o: or#arg2[B1, P2]#to[B1, PR]) =
      o.toRep(Stdlib./, arg1.tree, arg2.tree)

  }

  implicit def addSupPosNumericOps[B1](operand: Rep[SuperPos[B1]])(implicit ev2: UnliftedArgType[B1] with NumType) =
    new NumericOperations[B1, SuperPos[B1]](operand)
  implicit def addNumericOps[B1, T](operand: T)(implicit ev1: T => Rep[B1], ev2: UnliftedArgType[B1] with NumType) =
    new NumericOperations[B1, B1](operand)
}
