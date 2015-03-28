package fwb.parser.parsers

import fwb.parser.ast.AST._
import fwb.parser.ast.Constants.Constant
import fwb.parser.ast.Lst
import org.scalatest._
import scala.language.{existentials, reflectiveCalls}
import scalaz.NonEmptyList

/**
 * Created by Pietras on 27/03/15.
 */
class PcParserTest extends FunSpec with Matchers with Inside {
  val parser = {
    val pcparser = new PcParser with Tester
    trait Tester {
      this: PcParser =>
      type Parser[T] = theParser.Parser[T]
      val grammar = theParser

      def testParse[T](str: String)(p: Parser[T]) = {
        val phrase = theParser.phrase(p)
        val in = new theParser.PackratReader(new scala.util.parsing.input.CharArrayReader(str.toCharArray))
        phrase(in) match {
          case theParser.Success(t, _) => t
          case theParser.NoSuccess(msg, _) => throw new IllegalArgumentException(s"Could not parse '$str': $msg")
        }
      }
    }
    new {
      def parse[T](str: String)(implicit p: grammar.Parser[T]) = pcparser.testParse(str)(p)

      val grammar = pcparser.grammar
    }
  }

  private def assertFail[T](input: String)(implicit p: parser.grammar.Parser[T]) {
    an[IllegalArgumentException] should be thrownBy parser.parse(input)
  }

  describe("PcParser") {
    describe("expression") {
      it("should parse numeric literals") {
        implicit val p = parser.grammar.expression
        parser.parse("123") should equal(Literal(Constant(123)))
        parser.parse("3.14") should equal(Literal(Constant(3.14d)))
        assertFail("+123")
      }
      it("should parse string literals") {
        implicit val p = parser.grammar.expression
        parser.parse("\"hello\"") should equal(Literal(Constant("hello")))
        parser.parse("\"\"") should equal(Literal(Constant("")))
      }
      it("should parse bool literals") {
        implicit val p = parser.grammar.expression
        parser.parse("true") should equal(True)
        parser.parse("false") should equal(False)
      }
      it("should parse null literal") {
        implicit val p = parser.grammar.expression
        parser.parse("null") should equal(Null)
      }
      it("should parse list literal") {
        implicit val p = parser.grammar.expression
        inside(parser.parse("[1, 1+2, 3, 4]")) { case Literal(Lst(lst)) =>
          lst should have length 4
        }
        parser.parse("[]") should matchPattern { case Literal(Lst(List())) => }
      }
      it("should parse variable names") {
        implicit val p = parser.grammar.expression
        parser.parse("name_null") should equal(Identifier("name_null"))
        parser.parse("null_name") should equal(Identifier("null_name"))
        parser.parse("do_name")   should equal(Identifier("do_name"))
        parser.parse("nullname")  should equal(Identifier("nullname"))
        parser.parse("x123")      should equal(Identifier("x123"))

        assertFail("def")
      }
      it("should parse arithmetic expressions") {
        implicit val p = parser.grammar.expression
        parser.parse("1 + 1 * 2")   shouldBe a[Apply]
        parser.parse("1 / (2 - 2)") shouldBe a[Apply]
        parser.parse("(1 + 1) ^ 4") shouldBe a[Apply]
      }
      it("should parse rel expressions") {
        implicit val p = parser.grammar.expression
        parser.parse("1 == 1") should matchPattern { case Apply(Operator("=="), _) => }
        parser.parse("2 > 3")  should matchPattern { case Apply(Operator(">"), _) => }
        parser.parse("1 < 2")  should matchPattern { case Apply(Operator("<"), _) => }
        parser.parse("1 != 0") should matchPattern { case Apply(Operator("!="), _) => }
        parser.parse("1 >= 0") should matchPattern { case Apply(Operator(">="), _) => }
        parser.parse("1 <= 0") should matchPattern { case Apply(Operator("<="), _) => }
      }
      it("should parse bool arithmetic expressions") {
        implicit val p = parser.grammar.expression
        parser.parse("true && false")  should matchPattern { case Apply(Operator("and"), _) => }
        parser.parse("true and false") should matchPattern { case Apply(Operator("and"), _) => }
        parser.parse("true || false")  should matchPattern { case Apply(Operator("or"), _) => }
        parser.parse("false or true")  should matchPattern { case Apply(Operator("or"), _) => }
      }
      it("should parse arithmetic expressions with correct precedence") {
        implicit val p = parser.grammar.expression
        parser.parse("1 + 2 * 3 + 4 / 5 - 1*2 ^ 3") should equal(parser.parse("1 + (2 * 3) + (4 / 5) - (1 * (2 ^ 3))"))
      }
      it("should parse selection operator") {
        implicit val p = parser.grammar.expression
        parser.parse("a.b.c") should have(
          'seq (NonEmptyList(Identifier("a"), Identifier("b"), Identifier("c")))
        )
        parser.parse("a.b.1 + x.c") should matchPattern { case Apply(Operator("+"), _) => }
        parser.parse("3.14") should not be a[Select]
      }
      it("should parse $") {
        implicit val p = parser.grammar.expression
        parser.parse("a.$meta.b") should have(
          'seq (NonEmptyList(Identifier("a"), Identifier("$"), Identifier("meta"), Identifier("b")))
        )
      }
      it("should parse function call") {
        implicit val p = parser.grammar.expression
        inside(parser.parse("fun(x, y)")) { case Apply(Identifier("fun"), args) => args should have length 2 }
        parser.parse("fun(name1 = x, name2 = y, z)") should have(
          'names (List(Some("name1"), Some("name2"), None))
        )
        parser.parse("fun.ction(x, y, z)") shouldBe a [Select]
      }
    }
    describe("latin") {
      pending
    }
  }
}