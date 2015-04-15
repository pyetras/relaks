package fwb.dsl.ops

import fwb.dsl._
import AST._

import scala.language.implicitConversions

/**
 * Created by Pietras on 15/04/15.
 */
trait ListOps extends AnyOps with ToTypedTreeOps with Symbols {

  object List {
    def apply[T](xs: Rep[ArgType[T]]*)(implicit typ: ListType[T], ev: ArgType[T]) : Rep[ListType[T]] = {
      val t: Atom = ListConstructor(xs)(typ)
      new Rep[ListType[T]] {
        override def tree: TTree = t
      }
    }
  }

  implicit def listToRep[T](list: List[T])(implicit tpe: ListType[T]): Rep[ListType[T]] = new Rep[ListType[T]] {
    override def tree: Atom = Literal(list) //Literal is atom
  }

}
