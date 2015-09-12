package relaks.lang.dsl.extensions.ast

import com.typesafe.scalalogging.LazyLogging
import relaks.lang.ast._
import relaks.lang.dsl.AST.syntax._
import relaks.lang.dsl.Rep

import scala.language.implicitConversions
import scalaz.Scalaz._

/**
 * Created by Pietras on 15/04/15.
 */
trait Symbols extends LazyLogging { self =>

  private var definitions = Map.empty[Sym, Expression]
  private var symCounter = -1

  private object NoOpExpr extends Expression with Leaf

  class Sym(name_ : Int, expr_ : Expression) extends Atom {
    //kiama shit. sym class must have only one constructor, otherwise
    //an immense shitstorm is unleashed. if expr_ is a noop use the name_
    //parameter, otherwise generate one on my own from symCounter and save
    //expr_ to dictionary
    val name = expr_ match {
      case NoOpExpr => name_
      case _ =>
        symCounter += 1
        symCounter
    }

    expr_ match {
      case NoOpExpr =>
      case _ => logger.debug(s"rewrite $expr_ from $name_ to $name")
        saveDefinition(this, expr_)
//        replaceWith(expr_)
    }

    // TODO decide: either run everything as a query, make sym duplication create
    // new syms or make sym duplication replace old syms
    // simple duplication by copying just the name does not work well with kiama
    // since ancestor's syms are not being rewritten, they still point to the old
    // versions of the ancestors

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

    def replaceWith(expr: Expression): Sym = { //TODO: make this dependent on context
      assert(isDefined(this))
      saveDefinition(this, expr)
      Sym(name) //returns new sym to reset kiama's attribution TODO: kiama does an equality check, so the tree is actually not modified
    }

    private def toString_(that: Expression => String) = if (isDefined(this)) s"↗${that(findDefinition(this).get)}" else "↗?"

    override def mainToString: String = toString_(_.mainToString)
  }

  object Sym {
    def apply(name: Int) = new Sym(name, NoOpExpr)
    def unapply(sym: Sym) = sym.name.some
  }

  protected final def fresh: Sym = {
    symCounter += 1
    Sym(symCounter)
  }

  protected final def freshRep[T](typ: TType): Rep[T] = new Rep[T] {
    override val tree: Atom = fresh(typ)
  }

  private def saveDefinition(sym: Sym, expression: Expression) : Unit = {
    definitions += ((sym, expression))
  }

  private def isDefined(sym: Sym): Boolean = findDefinition(sym).isDefined

  protected def findDefinition(sym: Sym) : Option[Expression] = definitions.get(sym)

  implicit final def toAtom(expression: Expression) : Atom = expression match {
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

  object Fresh {
    def unapply(expr: Expression): Option[Sym] = {
      (Expr.unapply(expr), expr) match {
        case (None, s:Sym) => s.some
        case _ => None
      }
    }
  }

}
