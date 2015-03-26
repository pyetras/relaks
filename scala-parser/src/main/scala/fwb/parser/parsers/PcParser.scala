package fwb.parser.parsers

import fwb.parser.ast.AST
import scala.language.{postfixOps, existentials}
import scala.util.parsing.combinator._
import scala.util.matching.Regex

/**
 * Created by Pietras on 25/03/15.
 */
class PcParser extends FWBParser[String] {
  import AST._
  import scalaz._
  import Scalaz._

  override def parse(str: String) = {
    theParser(str).getOrElse(List())
  }

  private object theParser extends JavaTokenParsers with PackratParsers {
    def apply(str: String) : scalaz.Validation[String, Program] = {
      parseAll(program, str) match {
        case Success(tree, _) => scalaz.Success(tree)
        case NoSuccess(msg, _) => scalaz.Failure(msg)
      }
    }

    val keywords = NonEmptyList("def", "true", "false", "do", "end", "and", "or", "null")
    val latinKeywords = NonEmptyList("foreach", "filter", "limit", "join", "distinct", "order", "by", "sample",
                              "asc", "as", "desc", "load", "store", "and", "or", "search", "generate",
                              "null", "not", "grid", "in", "exists", "is")

    def keyword(str: String): Parser[String] = s"""$str\b""".r
    def latinKeyword(str: String): Parser[String] = s"""(?i)$str\b""".r

    def keywords2parsers[T](kws: NonEmptyList[T])(f: T => Parser[T]): Parser[T] = {
      val parsers = kws map f
      parsers.foldl1(acc => parser => acc | parser)
    }

    lazy val reserved = keywords2parsers(keywords)(keyword)
    lazy val reserved_latin = keywords2parsers(latinKeywords)(latinKeyword)

    implicit class KeywordOps(s: String) {
      def k = keyword(s)
      def ik = latinKeyword(s)
    }

    def eol : Parser[Unit] = {
      val parser = new Parser[Unit] {
        def apply(in: Input): ParseResult[Unit] = {
          case class SubSeq(offset: Int, len: Int) extends CharSequence {
            override def charAt(index: Int): Elem = in.source.charAt(index + offset)
            override def length(): Int = len
            override def subSequence(start: Int, end: Int): CharSequence = copy(offset = start + offset, len = end - start)
          }

          val whites = """[ \t]+""".r

          val in1 = whites findPrefixMatchOf SubSeq(in.offset, in.source.length() - in.offset) match {
            case Some(matched) => in.drop(matched.end)
            case None => in
          }

          def matchNewline(in: Input): ParseResult[Unit] = {
            val source = in.source
            val start = in.offset
            if (start < source.length() && (source.charAt(start) == '\n' || source.charAt(start) == ';'))
              Success((), in.drop(1))
            else {
              val found = if (start == source.length()) "end of source" else "`" + source.charAt(start) + "'"
              Failure(s"newline or ; expected but $found found", in)
            }
          }

          matchNewline(in1)
        }
      }
      parser <~ """\s*""".r
    }

    lazy val program : Parser[List[Statement]] = topStatement +

//    STATEMENTS
    lazy val topStatement: Parser[Statement] = statement
    lazy val statement   : Parser[Statement] = noop | (assignment <~ eol)
    lazy val noop        : Parser[Statement] = eol ^^^ NoOp

    lazy val assignment: Parser[Assignment] = (expression <~ "=") ~ expression ^^ {
      case left ~ right => Assignment(left, right)
    }

//    EXPRESSIONS
    lazy val expression: PackratParser[Expression] = identifier
    lazy val identifier: Parser[Expression] = identValue ^^ ( name => Identifier(name) )

    lazy val identValue: Parser[String] = not(reserved) ~> ident


  }

}
