package relaks.lang.dsl.extensions

import org.kiama.==>
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl._
import relaks.lang.dsl.extensions.ast.Symbols
import relaks.lang.phases.interpreter.BaseExprInterpreter
import shapeless._
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz._
import Scalaz._

import scala.language.implicitConversions
import scala.reflect.ClassTag

/**
 * Created by Pietras on 15/04/15.
 */
trait ListExtensions extends AnyExtensions with ASTSyntax with Symbols with TableExtensions with TupleExtensions {

  object List {
    def apply[T: ArgType](xs: Rep[T]*) : Rep[List[T]] = {
      val t: Atom = ListConstructor(xs.map(_.tree))(new ListType[T])
      new Rep[List[T]] {
        override val tree: Expression = t
      }
    }
  }

  implicit def listToRep[T: ArgType](list: List[T]): Rep[List[T]] = new Rep[List[T]] {
    override val tree: Atom = new Literal(list)(new ListType[T]) //Literal is atom
  }

  class ListOperations[T: ArgType](arg1: Rep[List[T]]) {
    def map[F: ArgType](f: Rep[T => F]): Rep[List[F]] = new Rep[List[F]] {
      override val tree = relaks.lang.ast.Apply(Stdlib.list_map, scala.List(arg1.tree, f.tree))(new ListType[F])
    }

    def asTable[H <: HList](implicit asTuple: AsTuple.Aux[T, H], mkCmp: BuildComprehension[TableRep[H], TableRep[H]]): Rep[TypedTable[Tup[H]]] = {
      val generator = Generator.fromFields(Vector(Field('x0, implicitly[ArgType[T]])))
      val fnarg = generator.toTuple[T :: HNil](0)
      val transform = Transform(generator, TableFromList(arg1.tree), SingleRow(asTuple(fnarg)).tree) //TODO change this to a list map when fn representations are available
      mkCmp(transform)
    }

  }

  implicit def listToListOps[T: ArgType](l: Rep[List[T]]): ListOperations[T] = new ListOperations[T](l)
}

trait ListInterpreter extends BaseExprInterpreter {
  protected val evalListExpression: Expression ==> EphemeralStream[Any] = {
    case _/> ListConstructor(exprs) => EphemeralStream.fromStream(exprs.toStream).map(e => evalExpression(e))
    case _/> Literal(l: List[_]) => EphemeralStream.apply(l: _*)
  }

  override def evalExpression(expr: TTree): Any = evalListExpression.applyOrElse(expr, super.evalExpression)
}
