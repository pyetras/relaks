package relaks.lang.dsl

import org.scalatest.{Inside, Matchers, FunSpec}
import relaks.lang.dsl.extensions.{TableComprehensionRewriter, TableExtensions, TableOps}
import shapeless._
import relaks.lang.dsl.AST._

/**
 * Created by Pietras on 22/05/15.
 */
class TableTest extends FunSpec with Matchers with Inside {
  describe("table extensions") {
    describe("syntax for untyped tables") {
      it("should construct ast for a simple map") {
        object Program extends DSL with TableExtensions
        import Program._
        val a = load("hello")
        val res = a(('x, 'y)).map { (t: Row2[Int, Int]) =>
          (1, 2)
        }
        analyze(res.tree)
//        println(res)

      }
      it("should construct ast for a nested flatmap") {
        object Program extends DSL with TableExtensions
        import Program._
        val a = load("hello")
        val b = load("world")
        val res = a(('x, 'y)).flatMap { (t: Row2[Int, Int]) =>
          b(('xx, 'yy)).map { (u: Row2[String, String]) =>
            (t(0), u(0))
          }
        }
        analyze(res.tree)
//        println(res)

      }

    }
    describe("ast rewriter") {
      it("should unnest nested comprehensions into joins") {
        object Program extends DSL with TableExtensions with TableComprehensionRewriter
        import Program._

        val a = load("hello")
        val b = load("world")
        val c = load("!!!")

        val res = for {
          as: Row2[Int, Int] <- a(('ax, 'ay))
          bs: Row2[Int, Int] <- b(('bx, 'by))
          cs: Row2[Int, Int] <- c(('cx, 'cy))
        } yield (as(0), as(1), bs(0), bs(1), cs(0), cs(1))

        val transformed = unnestTransforms(res.tree) //TODO: why does this not have a type
        analyze(transformed)
        transformed should matchPattern { case Expr(Transform(_, Expr(j: Join), Expr(_: Pure))) => }
      }
    }
  }
}
