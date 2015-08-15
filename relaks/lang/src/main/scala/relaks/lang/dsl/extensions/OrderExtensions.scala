package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl._
import scalaz.Order

import scala.language.{implicitConversions, reflectiveCalls}

/**
 * Created by Pietras on 16/04/15.
 */
trait OrderExtensions extends ASTSyntax with BoolExtensions with scalaz.std.AnyValInstances {
  class OrderOperations[B1](val arg1: Rep[B1]) extends Operations[B1] {
    def <(arg: Rep[B1]): Rep[Boolean] =
      op.arg2[B1].to[Boolean].toRep(Stdlib.<, arg1.tree, arg.tree)
    def <=(arg: Rep[B1]): Rep[Boolean] =
      op.arg2[B1].to[Boolean].toRep(Stdlib.<=, arg1.tree, arg.tree)
    def >(arg: Rep[B1]): Rep[Boolean] =
      op.arg2[B1].to[Boolean].toRep(Stdlib.>, arg1.tree, arg.tree)
    def >=(arg: Rep[B1]): Rep[Boolean] =
      op.arg2[B1].to[Boolean].toRep(Stdlib.>=, arg1.tree, arg.tree)

  }

  implicit def addOrderOps[B1: Order: ArgType](operand: Rep[B1]): OrderOperations[B1] =
    new OrderOperations[B1](operand)

}
