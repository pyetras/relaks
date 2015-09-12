package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl.AST.syntax._
import relaks.lang.dsl.Rep
import relaks.lang.dsl.extensions.ast.Symbols

import scalaz.syntax.Ops

/**
 * Created by Pietras on 16/04/15.
 */
trait UnaryStdlibSyntax extends Symbols {
  final implicit class UnaryStdlibOps(val self: Operator) extends Ops[Operator]{
    def toRep[T](args: Expression*)(implicit typ: ArgType[T]): Rep[T] =
      new Rep[T] {
        override val tree: Atom = Apply(self, args.toList)(typ)
      }
  }
}
