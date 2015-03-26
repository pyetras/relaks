package fwb.parser.ast

import fwb.parser.ast.Programs.Program

/**
 * Created by Pietras on 22/03/15.
 */
object Latins {
  abstract class Latin extends Expression
  sealed trait InferredRelation
  object InferredRelation extends InferredRelation

  type Relation = Either[InferredRelation, Expression]

  case class Foreach(rel: Relation, statements: Program) extends Latin
  case class Limit(rel: Relation, limiter: Expression) extends Latin

  sealed trait OrderDirection
  object Asc extends OrderDirection
  object Desc extends OrderDirection

  case class Order(rel: Relation, directions: List[(Relation, OrderDirection)]) extends Latin

  sealed trait SearchType
  case class Optimization(method: String = "spearmint") extends SearchType
  object Grid extends SearchType
  case class Search(rels: List[Relation], typ: SearchType, statements: Program) extends Latin

}
