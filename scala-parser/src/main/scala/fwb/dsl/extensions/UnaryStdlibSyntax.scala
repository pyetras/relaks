package fwb.dsl.extensions

import fwb.dsl.AST._
import fwb.dsl.AST.syntax._
import fwb.dsl.Rep
import scalaz.syntax.Ops

/**
 * Created by Pietras on 16/04/15.
 */
trait UnaryStdlibSyntax {
  final implicit class UnaryStdlibOps(val self: Operator) extends Ops[Operator]{
    def toRep[T](args: Expression*)(implicit typ: ArgType[T]): Rep[T] =
      new Rep[T] {
        override def tree: TTree = Apply(self, args.toList)(typ)
      }
  }
}
