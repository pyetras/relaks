package fwb.parser.ast

import fwb.utils.prolog.PrologList._
import jpl.Term

/**
 * Created by Pietras on 22/03/15.
 */
abstract class Expression extends ASTNode

object Expression {
  import fwb.parser.ast.literals._

  def apply(term: Term): Expression = {
    term.head.name() match {
      case "atomic" => {
        val rest = term.tail
        rest.head.name() match {
          case "var" => new Identifier(rest.tail.head)
          case "num_lit" => new Num(rest.tail.head)
          case "list_lit" => Lst(rest.tail.head)
          case "string_lit" => new Str(rest.tail.head)
          case "const" => {
            val value = rest.tail.head.name()
            value match {
              case "null" => throw new NotImplementedError()
              case value => Bool(value)
            }
          }
        }
      }
      case "math" => InfixOp(term.tail)
      case "bool_math" => InfixOp(term.tail)
    }
  }

  case class Identifier(name: String) extends Expression {
    def this(name: Term) = this(name.map(t => t.longValue().toChar).mkString)
    override def toString = name
  }

  case class InfixOp[+OP <: operators.Operator](operator: OP, left: Expression, right: Expression) extends Expression {
    override def toString = s"($left $operator $right)"
  }
  object InfixOp{
    def apply(term: Traversable[Term]) = {
      val terms = term.take(2)
      require(terms.head.isAtom())
      val op = operators(term.head.name())
      new InfixOp(op, Expression(terms.last.head), Expression(terms.last.last))
    }
  }

}
