package fwb.ast

import fwb.ast.Constants.Constant

import scala.reflect.ClassTag
import scalaz.NonEmptyList

/**
 * Created by Pietras on 26/03/15.
 */
trait ASTNodes extends Types {
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

  case class Identifier(name: String) extends Expression

  case class Literal(v: Value) extends Expression
  final val True = Literal(Constant(true))
  final val False = Literal(Constant(false))
  final val Null = Literal(Constant(null))

  sealed trait NondetChoice
  final case class ChoiceRange(from: Value, to: Value) extends NondetChoice
  final case class ChoiceList(s: Lst) extends NondetChoice

  case class Nondet(choice: NondetChoice) extends Expression

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
  final case class Operator(name: String) extends Expression

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