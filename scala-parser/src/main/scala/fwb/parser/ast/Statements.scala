package fwb.parser.ast

/**
 * Created by Pietras on 22/03/15.
 */
import jpl._

abstract class Statement extends ASTNode {
  override def toString = ";"
}

object Statement {
  def apply(term: Term) = {
    val name = term.args()(0).name()
    val rest = term.args()(1)
    name match {
      case "assign" => new Assignment(rest)
    }
  }

  case class Assignment(left: Expression, right: Expression) extends Statement {
    def this(rest: Term) = this(Expression(rest.args()(0)), Expression(rest.args()(1)))
    override def toString = s"$left = ${right}${super.toString}"
  }
}
