package fwb.dsl.extensions

import fwb.dsl._
import AST._

import scala.language.implicitConversions
import scala.language.reflectiveCalls

/**
 * Created by Pietras on 16/04/15.
 */
trait BoolExtensions extends ASTSyntax with UnaryStdlibSyntax {
  class BoolOperations[B1 : UnliftedArgType](val arg1: Rep[B1]) extends Operations[B1] {
    def unary_! = Stdlib.!.toRep[Boolean](arg1.tree)

    def ||(arg2: Rep[B1]): Rep[Boolean] = {
      op.arg2[B1].to[Boolean].toRep(Stdlib.||, arg1.tree, arg2.tree)
    }

    def &&(arg2: Rep[B1]): Rep[Boolean] = {
      op.arg2[B1].to[Boolean].toRep(Stdlib.&&, arg1.tree, arg2.tree)
    }
  }

  implicit def addBoolOps(operand: Rep[Boolean]): BoolOperations[Boolean] =
    new BoolOperations[Boolean](operand)

}
