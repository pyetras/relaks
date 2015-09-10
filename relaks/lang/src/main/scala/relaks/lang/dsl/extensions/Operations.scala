package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl._

import scala.language.implicitConversions

/**
 * Created by Pietras on 10/04/15.
 */
trait Operations[B1] {
  val arg1: Rep[B1]

  private[extensions] class RepConstructor[B1, B2, BR] { //this is useless
    import AST.syntax._
    def toRep(name: Expression, args: Expression*)(implicit tpe: ArgType[BR]): Rep[BR] = {
      new Rep[BR] {
        override val tree: Expression = Apply(name, args.toList)(tpe)
      }
    }
  }

  private[extensions] val op = new  {
    def to[BR] = new RepConstructor[B1, B1, BR]
    def arg2[B2] = new {
      def to[BR] = new RepConstructor[B1, B2, BR]
    }
  }

  protected val tree = arg1.tree

  implicit protected[this] def b1Type = tree.tpe.asInstanceOf[ArgType[B1]]
}
