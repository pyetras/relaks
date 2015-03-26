package fwb.parser.ast

import fwb.parser.ast.Constants.Constant

import scala.util.parsing.input.Positional
import scala.reflect.runtime.universe.Type

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

  abstract sealed class Tree extends Positional
  implicit class Program(val children: Traversable[Tree]) extends Tree
  object Program {
    def unapply(program: Program) = Some(program.children)
  }

  abstract sealed class Statement extends Tree
  case class Assignment(left: Expression, right: Expression) extends Statement
  object NoOp                                                extends Statement

  sealed trait Expression extends Tree {
    private[this] var rawtpe: Type = _
    final def tpe = rawtpe
  }

  case class Identifier(name: String)                          extends Expression

  case class Literal(c: Constant)                              extends Expression
  final val True = Literal(Constant(true))
  final val False = Literal(Constant(false))
  final val Null = Literal(Constant(null))

  case class Apply(fun: Expression, argList: List[Expression]) extends Expression

  final case class Operator(name: String)                      extends Expression

  abstract class Latin                                         extends Expression
  sealed trait InferredRelation
  object Inferred extends InferredRelation

  type Relation = Either[InferredRelation, Expression]

  case class Foreach(rel: Relation, statements: Program)                        extends Latin
  case class Limit(rel: Relation, limiter: Expression)                          extends Latin

  sealed trait OrderDirection
  object Asc extends OrderDirection
  object Desc extends OrderDirection

  case class Order(rel: Relation, directions: List[(Relation, OrderDirection)]) extends Latin

  sealed trait SearchType
  case class Optimization(method: String = "spearmint") extends SearchType
  object Grid extends SearchType
  case class Search(rels: List[Relation], typ: SearchType, statements: Program) extends Latin



}
