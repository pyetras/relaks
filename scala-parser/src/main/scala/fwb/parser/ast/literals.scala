package fwb.parser.ast

import jpl.Term
import fwb.utils.prolog.PrologList._

/**
 * Created by Pietras on 22/03/15.
 */
object literals {
  abstract class Literal[T] extends Expression {
    val value:T
    override def toString = value.toString
  }
  case class Num(value: Long) extends Literal[Long] {
    def this(term: Term) = this(term.longValue())
  }
  case class Lst[T](value: List[T]) extends Literal[List[T]]
  object Lst {
    def apply(term: Term) = {
      val list: List[Expression] = term.map(Expression(_)).toList
      new Lst(list)
    }
  }
  abstract class Bool extends Literal[Boolean]
  object Bool {
    def apply(value: String) : Bool = value match {
      case "true" => True
      case "false" => False
    }
  }
  object True extends Literal[Boolean] { val value = true }
  object False extends Bool { val value = false }
  case class Str(value: String) extends Literal[String] {
    def this(term: Term) = this(term.map(_.longValue().toChar).mkString)
    override def toString = '"' + super.toString + '"'
  }
}
