package fwb.parser.ast

/**
 * Created by Pietras on 22/03/15.
 */
import jpl._
abstract class ASTNode

object ASTNode {
  def apply(term: Term): ASTNode = {
    term.name() match {
      case "statement" => Statement(term)
      case "expression" => Expression(term)
    }

  }
}
