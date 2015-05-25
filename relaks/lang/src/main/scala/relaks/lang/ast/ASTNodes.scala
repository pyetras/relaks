package relaks.lang.ast

import org.kiama.attribution.Attributable

import scalaz.NonEmptyList

/**
 * Created by Pietras on 26/03/15.
 */
private[this] object syntax extends ToTypedTreeOps with ScalaTypeImplis
import syntax._

sealed trait Tree extends Typed with Attributable {
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

sealed trait Statement extends Tree
case class Assignment(left: Expression, right: Expression) extends Statement
case class Generate(exprs: NonEmptyList[Expression]) extends Statement
object NoOp extends Statement with Leaf

trait Expression extends Tree

trait Atom extends Expression

case class Identifier(name: String) extends Atom

trait Literal extends Atom with Leaf {
  type ValueT
  val value: Any

  override def mainToString = s"`${value.toString}`"

  override def equals(other: Any) = {
    other match {
      case lit@Literal(v) => v == value && lit.tpe == tpe
      case _ => false
    }
  }
}
object Literal {
  def apply[T](v: T)(implicit tpe: ArgType[T]): Literal = (new Literal {
    override type ValueT = T
    override val value = v
  })(tpe)

  def unapply(literal: Literal) = Some(literal.value)
}

//  object ListLiteral {
//    def unapply(literal: Literal): Option[List[Any]] = literal.tpe match {
//      case t:ListType[_] => Some(literal.value.asInstanceOf[List[Any]])
//      case _ => None
//    }
//  }

object Const {
  def apply[T](v: T) : Literal = new Literal {
    override type ValueT = T
    override val value: Any = v
  }
}

//  object True extends Literal{(true)
//  object False extends Literal(false)
//  object Null extends Literal(null)

sealed case class ListConstructor(lst: Seq[Expression]) extends Expression

sealed case class TupleConstructor(tuple: Vector[Expression]) extends Expression {
  val names = tuple.indices.map(i => s"x$i")
}

sealed trait NondetGenerator extends Expression
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

sealed trait Latin extends Expression
sealed trait InferredRelation
object Inferred extends InferredRelation

//  type Relation = Either[InferredRelation, Expression]
//
//  case class Foreach(rels: NonEmptyList[Relation], statements: NonEmptyList[Statement]) extends Latin
//  case class Limit(rel: Relation, limiter: Expression) extends Latin
//  case class Where(rel: Relation, condition: Expression) extends Latin
//
//  sealed trait OrderDirection
//  object Asc extends OrderDirection
//  object Desc extends OrderDirection
//
//  case class ColumnOrder(rel: Relation, directions: NonEmptyList[(Expression, OrderDirection)]) extends Latin
//
//  sealed trait SearchType
//  case class Optimization(method: String = "spearmint") extends SearchType
//  object Grid extends SearchType
//  case class Search(rels: NonEmptyList[Expression], typ: SearchType, statements: NonEmptyList[Statement]) extends Latin
