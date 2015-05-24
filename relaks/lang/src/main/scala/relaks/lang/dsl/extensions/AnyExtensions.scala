package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl._

import scala.language.{implicitConversions, reflectiveCalls}

/**
 * Created by Pietras on 15/04/15.
 */
trait AnyExtensions extends ASTSyntax with BoolExtensions {
  class AnyOperations[B1](val arg1: Rep[B1]) extends Operations[B1] {
    def === (arg2: Rep[B1]): Rep[Boolean] =
      op.arg2[B1].to[Boolean].toRep(Stdlib.==, arg1.tree, arg2.tree)

    def != (arg2: Rep[B1]): Rep[Boolean] =
      !(this === arg2)

    def !==(arg2: Rep[B1]) = arg1 != arg2

    def ==(arg2: Rep[B1]): Rep[Boolean] = throw new NotImplementedError("You probably meant to use ===")

  }

  implicit def anyToRep[B1](x: B1)(implicit tpe: ScalaType[B1]): Rep[B1] = new Rep[B1] {
    override val tree: Expression = Literal(x)
  }

  implicit def addAnyOps[B1](operand: Rep[B1])(implicit ev2: UnliftedArgType[B1]) =
    new AnyOperations[B1](operand)

}
