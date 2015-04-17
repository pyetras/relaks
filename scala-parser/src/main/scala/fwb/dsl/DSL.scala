package fwb.dsl

import fwb.dsl.AST.ASTSyntax
import fwb.dsl.ops._

import scala.annotation.implicitNotFound
import scala.collection.mutable
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/**
 * Created by Pietras on 14/04/15.
 */
trait DSL extends ASTSyntax with Symbols with SuperPosOps with AnyOps with BoolOps with OrderOps with NumericOps with ListOps with ProductOps {
  import AST._
  private var stored = new mutable.MutableList[Rep[Any]]

  @implicitNotFound("Cannot store a not determined value")
  def store[T](rep: Rep[T])(implicit ev: UnliftedArgType[T]) = stored += rep

  def showSpace[T](superPos: Rep[SuperPos[T]]): Option[String] = superPos.tree.tpe match {
    case _:SuperPosGenType[_] => superPos.tree match {
      case Expr(NondetChoiceList(Expr(ListConstructor(lst)))) => lst.toString().some
      case Expr(NondetChoiceList(Expr(Literal(x)))) => x.toString().some
      case Expr(NondetChoiceRange(l, r)) => s"{${l.value} .. ${r.value}}".some
      case _ => None
    }
    case _ => None
  }

}
