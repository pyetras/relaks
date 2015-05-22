package relaks.lang.dsl

import AST._
import AST.syntax._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable
import scala.language.implicitConversions
import scalaz._
import Scalaz._
import org.kiama.attribution.Attribution._

/**
 * Created by Pietras on 15/04/15.
 */
trait Symbols {

  private val definitions = new mutable.HashMap[Sym, Assignment]
  private var symCounter = 0

  sealed case class Sym(name: Int) extends Atom {

    override def productElement(n: Int): Any = {
      findDefinition(this).get
    }
    override def productArity: Int = if (isDefined(this)) 1 else 0

    override def canEqual(that: Any): Boolean = this.getClass == that.getClass

    override def equals(other: Any) = other match {
      case s:Sym => this.hashCode() == other.hashCode()
      case _ => false
    }
    override def hashCode() = name.hashCode()

    override def mainToString: String = if (isDefined(this)) s"↗${findDefinition(this).get.mainToString}" else "↗?"
  }

  protected def fresh: Sym = {
    val sym = Stream.from(symCounter).map((i) => Sym(i)) dropWhile definitions.contains
    symCounter = sym.head.name + 1
    sym.head
  }

  protected def freshRep[T](typ: TType): Rep[T] = new Rep[T] {
    override val tree: AST.Expression = fresh(typ)
  }

  private def saveDefinition(sym: Sym, expression: Expression) : Assignment = {
    val ass = Assignment(sym, expression)(expression.tpe)
//    initTree(ass) //TODO sure?
    definitions += ((sym, ass))
    ass
  }

  private def isDefined(sym: Sym): Boolean = definitions.contains(sym)

  private def findDefinition(sym: Sym) : Option[Expression] = definitions.get(sym).map(_.right)

  implicit def toAtom(expression: Expression) : Atom = expression match {
    case sym @ Sym(_) => sym
    case _ =>
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

  /**
   * use _/> ... instead of Expr(...) (less parentheses)
   */
  object /*_*//> {
    def unapply(expr: Expression) = Expr.unapply(expr).map(followed => ((), followed))
  }

}
