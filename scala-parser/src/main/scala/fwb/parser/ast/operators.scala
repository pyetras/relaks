package fwb.parser.ast

/**
 * Created by Pietras on 22/03/15.
 */
object operators {
  sealed abstract class Operator {
    val name: String
    override def toString = name
  }
  sealed abstract class BoolOperator extends Operator

  sealed abstract class MathOperator extends Operator
  object MulOperator extends MathOperator { val name = "*" }
  object DivOperator extends MathOperator { val name = "/" }
  object ModOperator extends MathOperator { val name = "%" }
  object AddOperator extends MathOperator { val name = "+" }
  object SubOperator extends MathOperator { val name = "-" }

  private val operators : Array[Operator] = Array(MulOperator, DivOperator, ModOperator, AddOperator, SubOperator)
  private val opMap = Map(operators.map(_.name).zip(operators): _*)

  def apply(op: String) : Operator = opMap(op)
}
