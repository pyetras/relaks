package fwb.dsl.ops

import fwb.dsl._
import AST._

import scala.language.implicitConversions

/**
 * Created by Pietras on 16/04/15.
 */
trait NumericOps extends ASTSyntax with Symbols {
  class NumericOperations[B1, P1](val operand: Rep[P1]) extends Operations[B1, P1] {
    def + [P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]) =
      o.toRep(Stdlib.+, operand.tree, arg.tree)

  }

  implicit def addSupPosNumericOps[B1, T](operand: T)(implicit ev1: T => Rep[SuperPos[B1]], ev2: UnliftedArgType[B1] with NumType) =
    new NumericOperations[B1, SuperPos[B1]](ev1(operand))
  implicit def addNumericOps[B1, T](operand: T)(implicit ev1: T => Rep[B1], ev2: UnliftedArgType[B1] with NumType) =
    new NumericOperations[B1, B1](operand)
}
