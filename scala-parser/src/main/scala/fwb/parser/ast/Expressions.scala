package fwb.parser.ast

import fwb.parser.ast.Constants.Constant

import reflect.runtime.universe.Type

/**
 * Created by Pietras on 22/03/15.
 */
trait Expression extends Tree {
  private[this] var rawtpe: Type = _
  final def tpe = rawtpe

}

object Expression {

  case class Identifier(name: String) extends Expression

  case class Literal(c: Constant) extends Expression

  case class Apply(fun: Expression, argList: List[Expression]) extends Expression

  final val True = Literal(Constant(true))
  final val False = Literal(Constant(false))
  final val Null = Literal(Constant(null))

  final case class Operator(name: String) extends Expression
}
