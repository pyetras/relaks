package relaks.lang.dsl

import org.scalatest.{FunSpec, Inside, Matchers}
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.extensions._
import shapeless._
import org.kiama.relation.GraphTree

import scalaz.{Failure, Success}

/**
 * Created by Pietras on 20/05/15.
 */
class SuperPosTest extends FunSpec with Matchers with Inside {
  def prog() = {
    object Program extends Symbols
    with SuperPosExtensions
    with AnyExtensions
    with BoolExtensions
    with OrderExtensions
    with NumericExtensions
    with ListExtensions
    with TupleExtensions
    with NativeFunExtensions
    with SuperPosAnalysis {
      override type Result = Unit

      override def run(res: Program.Result): Any = ???

      override def compile(expr: Expression): Program.Result = ???

      def superPosed(n: Expression): Boolean = new SuperPosed(new GraphTree(n)).isSuperPosed(n)
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

  it("should assign correct superPos attributes to native function calls") {
    val (p, _, _) = prog()
    import p._
    def f(x: Double, y: Double) = x + y
    val x: Rep[Double] = 1.0
    val y: Rep[Double] = 2.0
    val z1 = to (f _) apply (x, y)
    analyze(z1.tree)
    superPosed(z1.tree) should not be(true)

    val a = choose from List(1.0, 2.0, 3.0)
    val z2 = to (f _) apply (x, a)
    analyze(z2.tree)
    superPosed(z2.tree) should be(true)

    val z3 = to (f _) apply (a, z1)
    analyze(z3.tree)
    superPosed(z3.tree) should be(true)
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
