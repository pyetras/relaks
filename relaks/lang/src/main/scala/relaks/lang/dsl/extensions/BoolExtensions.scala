package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl._

import scala.language.{implicitConversions, reflectiveCalls}

/**
 * Created by Pietras on 16/04/15.
 */
trait BoolExtensions extends ASTSyntax with UnaryStdlibSyntax with OpUtils {
  class BoolOperations(val arg1: Rep[Boolean]) extends Operations[Boolean] {
    def unary_! = Stdlib.!.toRep[Boolean](arg1.tree)

    def ||(arg2: Rep[Boolean]): Rep[Boolean] = {
      op.arg2[Boolean].to[Boolean].toRep(Stdlib.||, arg1.tree, arg2.tree)
    }

    def &&(arg2: Rep[Boolean]): Rep[Boolean] = {
      op.arg2[Boolean].to[Boolean].toRep(Stdlib.&&, arg1.tree, arg2.tree)
    }
  }

  implicit def addBoolOps(operand: Rep[Boolean]): BoolOperations =
    new BoolOperations(operand)

}
