package fwb.dsl

import fwb.dsl.AST.ASTSyntax
import fwb.dsl.ops.{SuperPosMapperImplis, SuperPosOps, OperationsSyntax}

import scala.collection.mutable
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/**
 * Created by Pietras on 14/04/15.
 */
trait DSL extends OperationsSyntax with SuperPosMapperImplis with ASTSyntax with SuperPosOps {
  import AST._
  private var stored = new mutable.MutableList[Rep[Any]]
  private var definitions = new mutable.HashMap[Sym, Assignment]
  private var symCounter = 0

  sealed case class Sym(name: Int) extends Atom {
    override def equals(other: Any) = other match {
      case s:Sym => this.hashCode() == other.hashCode()
      case _ => false
    }
    override def hashCode() = name.hashCode()
  }

  def fresh: Sym = {
    val sym = Stream.from(symCounter).map((i) => Sym(i)) dropWhile definitions.contains
    symCounter = sym.head.name + 1
    sym.head
  }

  def saveDefinition(sym: Sym, expression: Expression) : Assignment = {
    val ass = Assignment(sym, expression)(expression.tpe)
    definitions += ((sym, ass))
    ass
  }

  def findDefinition(sym: Sym) : Option[Expression] = definitions.get(sym).map(_.right)

  implicit def toAtom(expression: Expression) : Atom = {
    val sym = fresh(expression.tpe)
    saveDefinition(sym, expression)
    sym
  }

  object Expr {
    def unapply(expr: Expression): Option[Expression] = expr match {
      case s:Sym => findDefinition(s)
      case _ => expr.some
    }
  }

  def store(rep: Rep[Any]) = stored += rep

  def showSpace[T](superPos: Rep[SuperPosArgType[T]]): Option[String] = superPos.tree.tpe match {
    case _:SuperPosGenType[_] => superPos.tree match {
      case Expr(NondetChoiceList(lst)) => lst.value.asInstanceOf[List[T]].toString().some
      case Expr(NondetChoiceRange(l, r)) => s"{${l.value} .. ${r.value}}".some
      case _ => throw new RuntimeException //TODO
    }
    case _ => None
  }

}
