package relaks.lang.parser.parsers

import jpl.Term
import relaks.lang.parser.AST._

/**
 * Created by Pietras on 24/03/15.
 */
class PrologParser extends FWBParser[Seq[Term]] with ScalaTypeImplis {
  import relaks.lang.prolog.PrologList._
  import scala.language.implicitConversions

  def parse(terms: Seq[Term]): Program = {
    Program(terms map buildStatement)
  }

  def buildStatement(term: Term) : Statement = {
    require(term.name() == "statement")
    val name = term.args()(0).name()
    val rest = term.args()(1)
    name match {
      case "assign" => Assignment(buildExpression(rest.args()(0)), buildExpression(rest.args()(1)))
    }
  }

  def buildExpression(term: Term): Expression = {
    require(term.name() == "expression")

    def infixOp(term: Traversable[Term]) = {
      val terms = term.take(2)
      require(terms.head.isAtom())
      val op = Operator(term.head.name())
      Apply(op, List(buildExpression(terms.last.head), buildExpression(terms.last.last)))
    }

    term.head.name() match {
      case "atomic" =>
        val rest = term.tail
        rest.head.name() match {
          case "var" =>
            val name = rest.tail.head
            Identifier(name.map(t => t.longValue().toChar).mkString)
          case "num_lit" =>
            val value = rest.tail.head.longValue()
            Literal(value)
          case "list_lit" =>
            val value = rest.tail.head.map(buildExpression).toList
            ListConstructor(value)
          case "string_lit" =>
            val value = rest.tail.head.map(_.longValue().toChar).mkString
            Literal(value)
          case "const" =>
            val value = rest.tail.head.name()
            value match {
              case "null" => Null
              case bool => bool match {
                case "true" => True
                case "false" => False
              }
            }
        }
      case "math" => infixOp(term.tail)
      case "bool_math" => infixOp(term.tail)
    }
  }

}
