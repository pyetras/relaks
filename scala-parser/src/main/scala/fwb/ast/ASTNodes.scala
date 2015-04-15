package fwb.ast

import scalaz.NonEmptyList

/**
 * Created by Pietras on 26/03/15.
 */
trait ASTNodes extends Types {
  private[this] object syntax extends ToTypedTreeOps with ScalaTypeImplis
  import syntax._

  sealed trait Tree extends Typed {
    def mainToString: String = this.getClass.getSimpleName
    override def toString = mainToString
  }
  implicit final class Program(val children: Traversable[Tree]) extends Tree
  object Program {
    def unapply(program: Program) = Some(program.children)
  }

  sealed trait Statement extends Tree
  case class Assignment(left: Expression, right: Expression) extends Statement
  case class Generate(exprs: NonEmptyList[Expression]) extends Statement
  object NoOp extends Statement

  sealed trait Expression extends Tree

  trait Atom extends Expression

  case class Identifier(name: String) extends Atom

  trait Literal extends Atom {
    type ValueT
    val value: Any

    override def equals(other: Any) = {
      other match {
        case lit@Literal(v) => v == value && lit.tpe == tpe
        case _ => false
      }
    }
  }
  object Literal {
    def apply[T](v: T)(implicit tpe: SimpleArgType[T]): Literal = (new Literal {
      override type ValueT = T
      override val value = v
    })(tpe)

    def unapply(literal: Literal) = Some(literal.value)
  }

  final val True = Literal(true)
  final val False = Literal(false)
  final val Null = Literal(null)

  sealed trait NondetChoice extends Expression
  case class NondetChoiceRange(from: Literal, to: Literal) extends NondetChoice
  case class NondetChoiceList(s: Literal) extends NondetChoice

  case class Apply(fun: Expression, argList: List[Expression]) extends Expression
  object Apply {
    def apply(s: String, args: Expression*): Apply = Apply(Operator(s), args.toList)
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

  type Relation = Either[InferredRelation, Expression]

  case class Foreach(rels: NonEmptyList[Relation], statements: NonEmptyList[Statement]) extends Latin
  case class Limit(rel: Relation, limiter: Expression) extends Latin
  case class Filter(rel: Relation, condition: Expression) extends Latin

  sealed trait OrderDirection
  object Asc extends OrderDirection
  object Desc extends OrderDirection

  case class Order(rel: Relation, directions: NonEmptyList[(Expression, OrderDirection)]) extends Latin

  sealed trait SearchType
  case class Optimization(method: String = "spearmint") extends SearchType
  object Grid extends SearchType
  case class Search(rels: NonEmptyList[Expression], typ: SearchType, statements: NonEmptyList[Statement]) extends Latin
}
