package fwb.parser.ast

/**
 * Created by Pietras on 22/03/15.
 */

abstract class Statement extends Tree

object Statement {
  case class Assignment(left: Expression, right: Expression) extends Statement
  object NoOp extends Statement
}
