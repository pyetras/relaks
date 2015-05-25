package relaks.lang.dsl

import com.typesafe.scalalogging.LazyLogging
import relaks.lang.ast._
import relaks.lang.dsl.AST.syntax._

import scala.collection.mutable
import scala.language.implicitConversions
import scalaz.Scalaz._

/**
 * Created by Pietras on 15/04/15.
 */
trait Symbols extends LazyLogging { self =>

  private val definitions = new mutable.HashMap[Sym, Assignment]
  private var symCounter = 0

  private object NoOpExpr extends Expression with Leaf

  class Sym(name_ : Int, expr_ : Expression) extends Atom {
    //kiama shit. sym class must have only one constructor, otherwise
    //an immense shitstorm is unleashed. if expr_ is a noop use the name_
    //parameter, otherwise generate one on my own from symCounter and save
    //expr_ to dictionary
    val name = expr_ match {
      case NoOpExpr => name_
      case _ =>
        val cnt = symCounter
        symCounter += 1
        cnt
    }

    expr_ match {
      case NoOpExpr =>
      case _ =>
        saveDefinition(this, expr_)
    }

    override def productElement(n: Int): Any = {
      if (n == 0) self //kiama shit. first element is the class scope
        else if (n == 1) name
        else
          findDefinition(this).get
    }
    override def productArity: Int = if (isDefined(this)) 3 else 2

    override def canEqual(that: Any): Boolean = this.getClass == that.getClass

    override def equals(other: Any) = other match {
      case s:Sym => this.hashCode() == other.hashCode()
      case _ => false
    }
    override def hashCode() = name.hashCode()

    private def toString_(that: Expression => String) = if (isDefined(this)) s"↗${that(findDefinition(this).get)}" else "↗?"

    override def mainToString: String = toString_(_.mainToString)
  }

  object Sym {
    def apply(name: Int) = new Sym(name, NoOpExpr)
    def unapply(sym: Sym) = sym.name.some
  }

  protected def fresh: Sym = {
    val sym = Stream.from(symCounter).map((i) => Sym(i)) dropWhile definitions.contains
    symCounter = sym.head.name + 1
    sym.head
  }

  protected def freshRep[T](typ: TType): Rep[T] = new Rep[T] {
    override val tree: Expression = fresh(typ)
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
    def unapply(expr: Expression): Option[(Option[Sym], Expression)] = {
      val symOpt = expr match {
        case s: Sym => Some(s)
        case _ => None
      }
      Expr.unapply(expr).map(followed => (symOpt, followed))
    }
  }

}
