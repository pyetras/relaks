package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl._

import scala.language.{implicitConversions, reflectiveCalls}

/**
 * Created by Pietras on 16/04/15.
 */
trait NumericExtensions extends ASTSyntax with Symbols {
  class NumericOperations[B1](val arg1: Rep[B1]) extends Operations[B1] {
    def + (arg2: Rep[B1]): Rep[B1] =
      op.arg2[B1].to[B1].toRep(Stdlib.+, arg1.tree, arg2.tree)

    def - (arg2: Rep[B1]): Rep[B1] =
      op.arg2[B1].to[B1].toRep(Stdlib.-, arg1.tree, arg2.tree)

    def * (arg2: Rep[B1]): Rep[B1] =
      op.arg2[B1].to[B1].toRep(Stdlib.*, arg1.tree, arg2.tree)

    def / (arg2: Rep[B1]): Rep[B1] =
      op.arg2[B1].to[B1].toRep(Stdlib./, arg1.tree, arg2.tree)

  }

  implicit def addNumericOps[B1](operand: Rep[B1])(implicit ev: UnliftedArgType[B1] with NumType) =
    new NumericOperations[B1](operand)
}

trait NumericContCompiler extends BaseContCompiler with Symbols {
  override def eval(expr: Expression, cont: (Any) => Cont): Cont = expr match {
    case Expr(Apply(Stdlib.+, a :: b :: Nil)) => eval(a, { case i1:Int => eval(b, { case i2:Int => cont(i1+i2)})})
    case _ => super.eval(expr, cont)
  }
}