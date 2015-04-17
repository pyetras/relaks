package fwb.dsl.extensions

import fwb.dsl._
import AST._

import scala.language.implicitConversions

/**
 * Created by Pietras on 16/04/15.
 */
trait BoolExtensions extends ASTSyntax with UnaryStdlibSyntax {
  class BoolOperations[B1 : UnliftedArgType, P1 : ArgType](val arg1: Rep[P1]) extends Operations[B1, P1] {
    def unary_! = Stdlib.!.toRep[P1](arg1.tree)

    def ||[P2, PR](arg2: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]) = {
      o.toRep(Stdlib.||, arg1.tree, arg2.tree)
    }

    def &&[P2, PR](arg2: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]) = {
      o.toRep(Stdlib.&&, arg1.tree, arg2.tree)
    }
  }

  implicit def addSupPosBoolOps(operand: Rep[Boolean])
                               (implicit ev1: Rep[Boolean] => Rep[SuperPos[Boolean]]) : BoolOperations[Boolean, SuperPos[Boolean]] =
    new BoolOperations[Boolean, SuperPos[Boolean]](ev1(operand))
  implicit def addBoolOps(operand: Rep[Boolean]): BoolOperations[Boolean, Boolean] =
    new BoolOperations[Boolean, Boolean](operand)

}
