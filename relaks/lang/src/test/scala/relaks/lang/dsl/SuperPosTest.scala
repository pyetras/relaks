package relaks.lang.dsl

import org.scalatest.{Inside, Matchers, FunSpec}
import relaks.lang.dsl.AST._
import relaks.lang.dsl.extensions._
import org.kiama.attribution.Attribution._
import shapeless.{HNil, ::}
import scalaz.{Success, Failure, ValidationNel}
import shapeless._

/**
 * Created by Pietras on 20/05/15.
 */
class SuperPosTest extends FunSpec with Matchers with Inside {
  def prog() = {
    object Program extends ASTSyntax
    with Symbols
    with SuperPosExtensions
    with AnyExtensions
    with BoolExtensions
    with OrderExtensions
    with NumericExtensions
    with ListExtensions
    with TupleExtensions
    with SuperPosAnalysis {
      override type Result = Unit

      override def run(res: Program.Result): Any = ???

      override def compile(expr: AST.Expression): Program.Result = ???

      def superPosed(n: TTree) = n.asInstanceOf[Expression] -> isSuperPosed
    }

    import Program._
    val a: Rep[Int] = 1
    val b = choose between 1 and 3
    (Program, a, b)
  }

  describe("SuperPos attribution") {
    it("should assign correct superPos attributes to simple types") {
      val (p, a, b) = prog()
      import p._

      analyze(a.tree)
      superPosed(a.tree) should not be(true)

      analyze(b.tree)
      superPosed(b.tree) should be(true)
    }

    it("should assign correct superPos attributes to expressions") {
      val (p, a, b) = prog()
      import p._

      val c = a + b
      analyze(c.tree)
      superPosed(c.tree) should be(true)

      val d = a === b
      analyze(d.tree)
      superPosed(d.tree) should be(true)

      val e = a + 1
      analyze(e.tree)
      superPosed(e.tree) should not be(true)
    }

    it("should follow sym links when assigning attributes") {
      val (p, a, b) = prog()
      import p._

      val f = a + b
      val s: Atom = f.tree.asInstanceOf[Expression]
      analyze(s)
      superPosed(s) should be(true)

      val g = (1:Rep[Int]) + (new Rep[Int]{
        override val tree: Atom = ((1:Rep[Int]) + ((choose between 1 and 3) + 1) + 5).tree.asInstanceOf[Expression](f.getTpe)
      }) + a + 10
      val s2: Atom = g.tree.asInstanceOf[Expression]
      analyze(s2)

      superPosed(s2) should be(true)
    }

    it("should assing correct superPos attributes to tuple types and operations") {
      val (p, a, _) = prog()
      import p._

      val tupA = {
        val x = choose between 1 and 3
        val t = (1, 4: Rep[Int], x, true)
        type TType = Int :: Int :: Int :: Boolean :: HNil
        val tup: Rep[Tup[TType]] = t
        analyze(tup.tree)
        tup
      }

      val tupB = {
        val t = (1, 4: Rep[Int], true)
        type TType = Int :: Int :: Boolean :: HNil
        val tup: Rep[Tup[TType]] = t
        analyze(tup.tree)
        tup
      }

      superPosed(tupA.tree) should be(true)
      superPosed(tupB.tree) should not be(true)

      //get concrete element - no need to generalize
      val getA1 = tupA(0)
      analyze(getA1.tree)
      superPosed(getA1.tree) should not be(true)

      val getA2 = tupA(2)
      analyze(getA2.tree)
      superPosed(getA2.tree) should be(true)

      //generalize superposed type
      val getA3 = tupA.at(a)
      analyze(getA3.tree)
      superPosed(getA3.tree) should be(true)

      val getB = tupB.at(a)
      analyze(getB.tree)
      superPosed(getB.tree) should not be(true)
    }
  }

  describe("SuperPos analyzer") {
    it("should validate the `once` operator") {
      val (p, a, b) = prog()
      import p._
      analyze(once(a).tree) should matchPattern { case Failure(_) => }
      analyze(once(b).tree) should matchPattern { case Success(()) => }
    }
  }
}
