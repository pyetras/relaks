package fwb.parser.parsers

import fwb.ast.Lst
import fwb.ast.Constants.Constant
import fwb.parser.AST
import scala.language.{postfixOps, existentials}
import scala.util.parsing.combinator._
import scalaz._


/**
 * Created by Pietras on 25/03/15.
 * TODO: pipes, functions (def), not, x[1], column as name
 */
class PcParser extends FWBParser[String]{
  import AST._

  override def parse(str: String) = {
    theParser(str).getOrElse(List())
  }

  protected object theParser extends JavaTokenParsers with PackratParsers with EolParser with OperatorPrecedenceParsers {
    import Scalaz._

    def apply(str: String) : scalaz.Validation[String, Program] = {
      val in = new PackratReader(new scala.util.parsing.input.CharArrayReader(str.toCharArray))
      parseAll(program, in) match {
        case Success(tree, _) => scalaz.Success(tree)
        case NoSuccess(msg, _) => {println(msg); scalaz.Failure(msg)}
      }
    }

    val keywords = NonEmptyList("def", "true", "false", "do", "end", "and", "or", "null")
    val latinKeywords = NonEmptyList("foreach", "filter", "limit", "join", "distinct", "order", "by", "sample",
                              "asc", "as", "desc", "load", "store", "and", "or", "search", "generate",
                              "null", "not", "grid", "in", "exists", "is")

    def keyword(str: String): Parser[String] = s"""$str\\b""".r
    def latinKeyword(str: String): Parser[String] = s"""(?i)$str\\b""".r

    def keywords2parsers[T](kws: NonEmptyList[T])(f: T => Parser[T]): Parser[T] = {
      val parsers = kws map f
      parsers.foldl1(acc => parser => acc | parser)
    }

    lazy val reserved = keywords2parsers(keywords)(keyword)
    lazy val reservedLatin = keywords2parsers(latinKeywords)(latinKeyword)

    implicit class KeywordOps(s: String) {
      def k = keyword(s)
      def ki = latinKeyword(s)
    }

    def rep1sep[T,U](p: Parser[T], sep: Parser[U]): Parser[NonEmptyList[T]] = p ~ rep(sep ~> p) ^^
      { case h ~ lst => h.wrapNel :::> lst }

    lazy val program : Parser[List[Statement]] = topStatement +

//    STATEMENTS
    lazy val topStatement: Parser[Statement] = statement
    lazy val statement   : Parser[Statement] = noop | (assignment <~ eol)
    lazy val noop        : Parser[Statement] = eol ^^^ NoOp

    lazy val assignment: Parser[Assignment] = (expression <~ "=") ~ expression ^^ {
      case left ~ right => Assignment(left, right)
    }

//    EXPRESSIONS
    lazy val expression: PackratParser[Expression] = latinExpr | arith
    lazy val identifier: Parser[Expression] = not(reserved) ~> identValue

    def infixOp(name: String, priority: Double, matcher: Option[Parser[String]] = None)(implicit ev: String => Parser[String]): Op[String, Expression] =
      Infix(priority, priority-1)(matcher.getOrElse(ev(name))) { (_, lhs, rhs) => Apply(Operator(name), List(lhs, rhs))}

    lazy val cmpOps = List(
      infixOp("<=", 600),
      infixOp(">=", 600),
      infixOp("<", 600),
      infixOp(">", 600)
    )

    lazy val mathOps = List(
      infixOp("+", 500),
      infixOp("-", 500),
      infixOp("*", 400),
      infixOp("/", 400),
      infixOp("^", 300),
      infixOp("mod", 400, Some("%" | "mod"))
    )

    lazy val boolOps = List(
      infixOp("and", 800, Some("and" | "&&")),
      infixOp("or", 800, Some("or" | "||"))
    )

    lazy val arith: PackratParser[Expression] = operators[String, Expression](
      List(infixOp("==", 700), infixOp("!=", 700)) ::: cmpOps ::: mathOps ::: boolOps:_*
    )(simpleExpr)

    lazy val parens: Parser[Expression] = "(" ~> expression <~ ")"

    lazy val simpleExpr: PackratParser[Expression] = call | special | select | atomicExpr

    lazy val select: PackratParser[Expression] = operators[String, Expression](
      Infix(100 - 1, 100)(".") { (_, lhs, rhs) => Select(lhs, rhs) }
    )(simpleExpr)

    lazy val special = "$" ~> (select | atomicExpr) ^^ (rhs => Select(Identifier("$"), rhs))

    lazy val atomicExpr: PackratParser[Expression] = parens | listLit | numLit | stringLit | boolLit | nullLit | identifier

    lazy val stringLit = stringLiteral ^^ (str => Literal(Constant(str.stripPrefix("\"").stripSuffix("\""))))

    lazy val numLit: Parser[Literal] = double | integer
    lazy val integer = wholeNumber ^^ ((num:String) => Literal(Constant(num.toInt)))
    lazy val double = floatingPointNumber ^^ ((num:String) => Literal(Constant(num.toDouble)))

    lazy val listLit: Parser[Literal] = "[" ~> repsep(expression, ",") <~ "]" ^^ (x => Literal(Lst(x)))

    lazy val boolLit: Parser[Literal] = ("true".k ^^^ True) | ("false".k ^^^ False)

    lazy val nullLit: Parser[Literal] = "null".k ^^^ Null

    lazy val call: PackratParser[Apply] = (expression <~ "(") ~ repsep(callArg, ",") <~ ")" ^^
      { case fun~args => {
        val namez = args.map(_._1)
        val vals = args.map(_._2)
        new Apply(fun, vals) with NamedArgs {
          val names = namez
        }
      }}
    lazy val callArg: PackratParser[(Option[String], Expression)] = ((ident <~ "=")?) ~ expression ^^
      {case l~r => (l, r)}

    //    LATIN
    lazy val latinExpr: PackratParser[Expression] = foreach | limit | filter | order | search | load | store

    lazy val latinIdent: PackratParser[Expression] = quotedIdent | (not(reservedLatin) ~> identValue)
    // `identifier.1`
    lazy val quotedIdent: Parser[Expression] =
      ("`"+"""([^`\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+"""+"`").r ^^ Identifier

    lazy val colList: PackratParser[NonEmptyList[Expression]] = rep1sep(column, ",")
    lazy val column = expression

    lazy val foreach: Parser[Foreach] = "foreach".ki ~> colList ~ stmtsWithGenerate ^^
      { case exprs~stmts => Foreach(exprs.map(x => Right(x)), stmts)}
    lazy val stmtsWithGenerate: PackratParser[NonEmptyList[Statement]] = (statement *) ~ generate ^^
      { case Nil~gen => gen.wrapNel
        case (h::t)~gen => h.wrapNel :::> t append gen.wrapNel
      }
    lazy val generate: Parser[Statement] = "generate".ki ~> exprList ^^ (cols => Generate(cols))
    lazy val exprList: Parser[NonEmptyList[Expression]] = rep1sep(expression, ",")

    lazy val limit: Parser[Limit] = "limit".ki ~> column ~ expression ^^ { case col~expr => Limit(Right(col), expr)}

    lazy val filter: Parser[Filter] = "filter".ki ~> column ~ ("by".ki ~> expression) ^^
      { case col~expr => Filter(Right(col), expr)}

    lazy val order: Parser[AST.Order] = "order".ki ~> column ~ ("by".ki ~> directions) ^^
      { case col ~ dirs => AST.Order(Right(col), dirs) }
    lazy val directions: Parser[NonEmptyList[(Expression, OrderDirection)]] = rep1sep(column ~ ((asc | desc)?), ",") ^^
      ((nel) => nel.map ((x:Expression ~ Option[OrderDirection]) => x match {
        case col ~ Some(ord) => (col, ord)
        case col ~ None => (col, Asc)
      }))
    lazy val asc: Parser[OrderDirection] = "asc".ki ^^^ Asc
    lazy val desc: Parser[OrderDirection] = "desc".ki ^^^ Desc

    lazy val search: PackratParser[Search] = "grid".ki.? ~ ("search".ki ~> colList) ~ stmtsWithGenerate ^^
      { case Some("grid") ~ cols ~ stmts => Search(cols, Grid, stmts)
        case None ~ cols ~ stmts => Search(cols, Grid, stmts)
      }

    lazy val load: Parser[AST.Apply] = "load".ki ~> io ^^
      ( path => AST.Apply(Identifier("load"), List(Literal(Constant(path)))) )
    lazy val store: Parser[AST.Apply] = "store".ki ~> io ^^
      ( path => AST.Apply(Identifier("store"), List(Literal(Constant(path)))) )
    lazy val io = stringLiteral

    lazy val identValue: Parser[Identifier] = ident ^^ ( name => Identifier(name) )
  }

}
