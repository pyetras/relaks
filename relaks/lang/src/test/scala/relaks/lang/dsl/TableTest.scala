package relaks.lang.dsl

import com.fasterxml.jackson.databind.ObjectMapper
import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FunSpec, Inside, Matchers}
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.extensions.ast._
import relaks.lang.dsl.extensions.{DrillCompiler, TableComprehensionRewriter, TableExtensions}
import shapeless._

/**
 * Created by Pietras on 22/05/15.
 */
class TableTest extends FunSpec with Matchers with Inside with LazyLogging {
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

      it("should construct a filter expression") {
        object Program extends DSL with TableExtensions
        import Program._
        val a = load("hello")
        val res = a(('x, 'y)).filter { (t: Row2[Int, Int]) =>
          t(0) === 1
        }

        analyze(res.tree)

        res.tree should matchPattern { case Expr(Filter(_, _, _/>Apply(Stdlib.==, _))) => }
      }

      it("should construct a filter expression from a for comprehension") {
        object Program extends DSL with TableExtensions
        import Program._
        val a = load("hello")
        val res = for {
          as: Row2[Int, Int] <- a(('x, 'y)) if as(0) === 1
        } yield (as(0), 1)

        analyze(res.tree)

        res.tree should matchPattern { case Expr(Transform(_, Expr(Filter(_, _, _/>Apply(Stdlib.==, _))), _)) => }
      }

    }
    describe("ast rewriter") {
      it("should unnest nested comprehensions into joins") {
        object Program extends DSL with TableExtensions with TableComprehensionRewriter
        import Program._
        import org.kiama.rewriting.Rewriter._

        val a = load("hello")
        val b = load("world")
        val c = load("!!!")

        val res = for {
          as: Row2[Int, Int] <- a(('ax, 'ay))
          bs: Row2[Int, Int] <- b(('bx, 'by))
          cs: Row2[Int, Int] <- c(('cx, 'cy))
        } yield (as(0), as(1), bs(0), bs(1), cs(0), cs(1))

        val strategy = repeat(oncetd(unnestTransforms))
//        analyze(res.tree)
        val transformed = strategy(res.tree).get.asInstanceOf[TTree]
//        analyze(transformed)

        transformed should matchPattern { case _/>Transform(_, _/>Join(_, _/>Join(_, _, CartesianJoin, _), CartesianJoin, _), _/>(_: Pure)) => }
      }

      it("should unnest nested comprehensions with filters into non-cartesian joins") {
        object Program extends DSL with TableExtensions with TableComprehensionRewriter
        import Program._
        import org.kiama.rewriting.Rewriter._

        val a = load("hello")
        val b = load("world")

        val res = for {
          as: Row2[Int, Int] <- a(('ax, 'ay))
          bs: Row2[Int, Int] <- b(('bx, 'by)) if as(0) === bs(0)
        } yield (as(1), bs(1))

        val strategy = repeat(oncetd(unnestTransforms))
        analyze(res.tree)
        val transformed = strategy(res.tree).get.asInstanceOf[TTree]
        transformed should matchPattern { case _/>Transform(_, _/>Join(_, _, InnerJoin, Some((_, _/>Apply(Stdlib.==, _)))), _/>(_: Pure)) => }
      }

      it("should unnest multiple comprehensions with filters into non-cartesian joins") {
        object Program extends DSL with TableExtensions with TableComprehensionRewriter
        import Program._
        import org.kiama.rewriting.Rewriter._

        val a = load("hello")
        val b = load("world")
        val c = load("!!!")

        val res = for {
          as: Row2[Int, Int] <- a(('ax, 'ay))
          bs: Row2[Int, Int] <- b(('bx, 'by))
          cs: Row2[Int, Int] <- c(('cx, 'cy)) if as(0) === bs(0) && bs(0) === cs(0)
        } yield (as(0), as(1), bs(0), bs(1), cs(0), cs(1))

        val strategy = repeat(oncetd(unnestTransforms))
        analyze(res.tree)
        val transformed = strategy(res.tree).get.asInstanceOf[TTree]
        analyze(transformed)
        println(transformed.verboseString)
        transformed should matchPattern { case _/>Transform(_, _/>Join(_, _/>Join(_, _, InnerJoin, _), InnerJoin, _), _/>(_: Pure)) => }
      }
    }
    describe("drill compiler") {
      it("should compile a load table expression") {
//        object Program extends TableExtensions with TableComprehensionRewriter with DrillCompiler
//
//        val table = Program.load("here")
//        val mapper = new ObjectMapper()
//        val obj = Program.compile(table.tree)
//        println(mapper.writeValueAsString(obj))
      }
    }
  }
}
