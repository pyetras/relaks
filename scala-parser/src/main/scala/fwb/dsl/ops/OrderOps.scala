package fwb.dsl.ops

import fwb.dsl._
import AST._

import scala.language.implicitConversions
import scalaz.Order

/**
 * Created by Pietras on 16/04/15.
 */
trait OrderOps extends ASTSyntax with BoolOps with SuperPosMapperImplis {
  class OrderOperations[B1, P1](val arg1: Rep[P1]) extends Operations[B1, P1] {
    def <[P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]): Rep[PR] =
      o.toRep(Stdlib.<, arg1.tree, arg.tree)
    def <=[P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]): Rep[PR] =
      o.toRep(Stdlib.<=, arg1.tree, arg.tree)
    def >[P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]): Rep[PR] =
      o.toRep(Stdlib.>, arg1.tree, arg.tree)
    def >=[P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR], ops: Rep[PR] => BoolOperations[P1, B1]) =
      !(this < arg)

  }

  implicit def addSupPosOrderOps[B1](operand: Rep[B1])
                                    (implicit ev1: Rep[B1] => Rep[SuperPos[B1]], ev2: UnliftedArgType[B1], o: Ordering[B1]): OrderOperations[B1, SuperPos[B1]] =
    new OrderOperations[B1, SuperPos[B1]](ev1(operand))
  implicit def addOrderOps[B1](operand: Rep[B1])(implicit ev2: UnliftedArgType[B1], o: Ordering[B1]) =
    new OrderOperations[B1, B1](operand)

}
