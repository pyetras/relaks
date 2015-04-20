package fwb.dsl

import AST._
import AST.syntax._

import scala.collection.mutable
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/**
 * Created by Pietras on 15/04/15.
 */
trait Symbols {

  private val definitions = new mutable.HashMap[Sym, Assignment]
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

}
