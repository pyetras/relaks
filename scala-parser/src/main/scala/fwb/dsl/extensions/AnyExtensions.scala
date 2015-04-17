package fwb.dsl.extensions

import fwb.dsl._
import AST._

import scala.language.implicitConversions

/**
 * Created by Pietras on 15/04/15.
 */
trait AnyExtensions extends ASTSyntax with BoolExtensions {
  class AnyOperations[B1, P1](val arg1: Rep[P1]) extends Operations[B1, P1] {
    def === [P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR]) : Rep[PR] =
      o.toRep(Stdlib.==, arg1.tree, arg.tree)
    def != [P2, PR](arg: Rep[P2])(implicit o: or#arg2[B1, P2]#to[Boolean, PR], ops: Rep[PR] => BoolOperations[P1, B1]) =
      !(this === arg)

  }

  implicit def anyToRep[B1](x: B1)(implicit tpe: ScalaType[B1]): Rep[B1] = new Rep[B1] {
    override def tree: TTree = Literal(x)
  }

  implicit def addSupPosAnyOps[B1](operand: Rep[B1])(implicit ev1: Rep[B1] => Rep[SuperPos[B1]], ev2: UnliftedArgType[B1]) =
    new AnyOperations[B1, SuperPos[B1]](ev1(operand))
  implicit def addAnyOps[B1, T](operand: Rep[B1])(implicit ev2: UnliftedArgType[B1]) =
    new AnyOperations[B1, B1](operand)

}
