package fwb.dsl.ops

import fwb.dsl._
import AST._

import scala.language.implicitConversions

/**
 * Created by Pietras on 16/04/15.
 */
trait BoolOps extends ASTSyntax {
  class BoolOperations[B1, P1](val operand: Rep[P1]) extends Operations[B1, P1] {
    def <[P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]) =
      o.toRep(Stdlib.<, operand.tree, arg.tree)

  }

  implicit def addSupPosOrderOps(operand: Rep[Boolean])(implicit ev1: Rep[Boolean] => Rep[SuperPos[Boolean]]) =
    new BoolOperations[Boolean, SuperPos[Boolean]](ev1(operand))
  implicit def addOrderOps[T](operand: T)(implicit ev1: T => Rep[ScalaType[Boolean]]) =
    new BoolOperations[ScalaType[Boolean], ScalaType[Boolean]](operand)

}
