package relaks.lang.ast

import relaks.optimizer.NondetParam

import scalaz.NonEmptyList

/**
 * Created by Pietras on 26/03/15.
 */
private[this] object syntax extends ToTypedTreeOps with ScalaTypeImplis
import syntax._

sealed trait Tree extends Typed with Product with Cloneable {
  def mainToString: String = this.getClass.getSimpleName
  protected def withArgs(main: String, args: String*) = s"${main}[${args.mkString(", ")}]"
  //    override def toString: String = mainToString //already overridden in Typed
}

trait Leaf extends Product {
  override def productElement(n: Int): Any = throw new IndexOutOfBoundsException(n.toString)
  override def productArity: Int = 0
  override def canEqual(that: Any): Boolean = this.getClass == that.getClass
}

sealed case class Program(stmts: Seq[Tree]) extends Tree

trait Expression extends Tree

trait Atom extends Expression

case class Identifier(name: String) extends Atom

class Literal(val value: Any) extends Atom with Leaf {
  override def mainToString = s"`${value.toString}`"

  override def equals(other: Any) = {
    other match {
      case lit@Literal(v) => v == value && lit.tpe == tpe
      case _ => false
    }
  }
}
object Literal {
  def apply[T](v: T)(implicit tpe: ArgType[T]): Literal = (new Literal(v))(tpe)

  def unapply(literal: Literal) = Some(literal.value)
}

case class Native(value: Any) extends Atom with Leaf

//  object ListLiteral {
//    def unapply(literal: Literal): Option[List[Any]] = literal.tpe match {
//      case t:ListType[_] => Some(literal.value.asInstanceOf[List[Any]])
//      case _ => None
//    }
//  }

//  object True extends Literal{(true)
//  object False extends Literal(false)
//  object Null extends Literal(null)

sealed case class ListConstructor(lst: Seq[Expression]) extends Expression

sealed case class TupleConstructor(tuple: Vector[Expression], names: Vector[String]) extends Expression

object TupleConstructor {
  def apply(tuple: Vector[Expression]): TupleConstructor = TupleConstructor(tuple, tuple.indices.map(i => s"x$i").toVector)
  def unapply(expr: Expression) = expr match {
    case t: TupleConstructor => Some(t.tuple)
    case _ => None
  }
}

object TupleWithNames {
  import scalaz.Scalaz._
  def unapply(expr: Expression): Option[(Vector[Expression], Vector[String])] = expr match {
    case t: TupleConstructor => Some((t.tuple, t.names))
    case _ => None
  }
  def unapplyWithTypes(row: Expression): Option[Vector[(String, TType)]] = unapply(row)
    .map(_.zipped.map { case (expr, name) => (name, expr.tpe)})
}

sealed trait NondetGenerator extends Expression {
  val name: String = "x"
}
sealed case class NondetGeneratorRange(from: Literal, to: Literal) extends NondetGenerator
sealed case class NondetGeneratorList(s: Expression) extends NondetGenerator

sealed case class Once(a: Atom) extends Expression

sealed case class Apply(fun: Expression, argList: List[Expression]) extends Expression
object Apply {
  def apply(s: String, args: Expression*): Apply = Apply(Operator(s), args.toList)
}

sealed class ApplyNamed(fn: Expression, val names: List[Option[String]], args: Expression*)
  extends Apply(fn, args.toList) with NamedArgs {
}

sealed case class ApplyNative(fn: Any, argTuple: Expression) extends Expression

trait NamedArgs { this: Apply =>
  val names: List[Option[String]]
}

case class Select(lhs: Expression, rhs: Expression) extends Expression {
  val seq: NonEmptyList[Expression] = rhs match {
    case s: Select => lhs <:: s.seq
    case _ => NonEmptyList(lhs, rhs)
  }
}
sealed case class Operator(name: String) extends Expression
