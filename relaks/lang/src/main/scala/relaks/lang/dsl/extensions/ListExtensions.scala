package relaks.lang.dsl.extensions

import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl._
import relaks.lang.dsl.extensions.ast.Symbols
import shapeless.HNil

import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
 * Created by Pietras on 15/04/15.
 */
trait ListExtensions extends AnyExtensions with ASTSyntax with Symbols with TableExtensions {

  object List {
    def apply[T](xs: Rep[T]*)(implicit typ: ListType[T]) : Rep[List[T]] = {
      val t: Atom = ListConstructor(xs.map(_.tree))(typ)
      new Rep[List[T]] {
        override val tree: Expression = t
      }
    }
  }

  implicit def listToRep[T: ClassTag](list: List[T])(implicit tpe: ListType[T]): Rep[List[T]] = new Rep[List[T]] {
    override val tree: Atom = Literal(list) //Literal is atom
  }

  class ListOperations[T](arg1: Rep[List[T]]) {
    import shapeless.::
    def asTable() = new ProjectedTypedTableComprehensions[T :: HNil](Vector(Field('x0, UnknownType)), TableFromList(arg1.tree)) //TODO typ
  }

  implicit def listToListOps[T](l: Rep[List[T]]): ListOperations[T] = new ListOperations[T](l)


}
