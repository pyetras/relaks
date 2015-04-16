package fwb.dsl.ops

import fwb.dsl._
import AST._

import scala.language.implicitConversions
import scalaz.Order

/**
 * Created by Pietras on 16/04/15.
 */
trait OrderOps extends ASTSyntax {
  class OrderOperations[B1, P1](val operand: Rep[P1]) extends Operations[B1, P1] {
    def <[P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]) =
      o.toRep(Stdlib.<, operand.tree, arg.tree)

  }

  implicit def addSupPosOrderOps[B1](operand: Rep[B1])(implicit ev1: Rep[B1] => Rep[SuperPos[B1]], ev2: LiftedArgType[B1], o: Order[B1]) =
    new OrderOperations[B1, SuperPos[B1]](ev1(operand))
  implicit def addOrderOps[B1](operand: Rep[B1])(implicit ev2: LiftedArgType[B1], o: Order[B1]) =
    new OrderOperations[B1, B1](operand)

}
