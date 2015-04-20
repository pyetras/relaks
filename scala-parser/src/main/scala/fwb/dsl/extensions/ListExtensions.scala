package fwb.dsl.extensions

import fwb.dsl._
import AST._

import scala.language.implicitConversions

/**
 * Created by Pietras on 15/04/15.
 */
trait ListExtensions extends AnyExtensions with ASTSyntax with Symbols {

  object List {
    def apply[T](xs: Rep[T]*)(implicit typ: ListType[T]) : Rep[List[T]] = {
      val t: Atom = ListConstructor(xs)(typ)
      new Rep[List[T]] {
        override val tree: Expression = t
      }
    }
  }

  implicit def listToRep[T](list: List[T])(implicit tpe: ListType[T]): Rep[List[T]] = new Rep[List[T]] {
    override val tree: Atom = Literal(list) //Literal is atom
  }

}