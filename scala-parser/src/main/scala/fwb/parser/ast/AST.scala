package fwb.parser.ast

import fwb.parser.ast.Constants.Constant

import scala.util.parsing.input.Positional
import scala.reflect.runtime.universe.Type
import scalaz.NonEmptyList

/**
 * Created by Pietras on 26/03/15.
 */
object AST {
//  trait ast {
//    type Tree = Tree
//    type Program = Program
//    type Statement = Statement
//    type Assignment = Assignment
//    type Expression = Expression
//    type Identifier = Identifier
//  }

  sealed trait Tree extends Positional
  implicit final class Program(val children: Traversable[Tree]) extends Tree
  object Program {
    def unapply(program: Program) = Some(program.children)
  }

  sealed trait Statement extends Tree
  case class Assignment(left: Expression, right: Expression) extends Statement
  case class Generate(exprs: NonEmptyList[Expression])        extends Statement
  object NoOp                                                extends Statement

  sealed trait Expression extends Tree {
    private[this] var rawtpe: Type = _
    final def tpe = rawtpe
  }

  case class Identifier(name: String)                          extends Expression

  case class Literal(v: Value)                              extends Expression
  final val True = Literal(Constant(true))
  final val False = Literal(Constant(false))
  final val Null = Literal(Constant(null))

  case class Apply(fun: Expression, argList: List[Expression]) extends Expression
  trait NamedArgs { this: Apply =>
    val names: List[Option[String]]
  }

  case class Select(lhs: Expression, rhs: Expression)          extends Expression
  final case class Operator(name: String)                      extends Expression

  sealed trait Latin                                           extends Expression
  sealed trait InferredRelation
  object Inferred extends InferredRelation

  type Relation = Either[InferredRelation, Expression]

  case class Foreach(rels: NonEmptyList[Relation], statements: NonEmptyList[Statement])       extends Latin
  case class Limit(rel: Relation, limiter: Expression)                          extends Latin
  case class Filter(rel: Relation, condition: Expression)                       extends Latin

  sealed trait OrderDirection
  object Asc extends OrderDirection
  object Desc extends OrderDirection

  case class Order(rel: Relation, directions: NonEmptyList[(Expression, OrderDirection)]) extends Latin

  sealed trait SearchType
  case class Optimization(method: String = "spearmint") extends SearchType
  object Grid extends SearchType
  case class Search(rels: NonEmptyList[Expression], typ: SearchType, statements: NonEmptyList[Statement]) extends Latin
}
