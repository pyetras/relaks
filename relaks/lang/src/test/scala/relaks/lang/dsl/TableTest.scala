package relaks.lang.dsl

import com.fasterxml.jackson.databind.ObjectMapper
import com.typesafe.scalalogging.LazyLogging
import org.apache.drill.common.logical.data.{Transform => DrillTransform, LogicalOperator, Scan, Join => DrillJoin}
import org.scalatest.enablers.Collecting
import org.scalatest._
import relaks.lang.ast._
import relaks.lang.dsl.AST._
import relaks.lang.dsl.extensions.ast.Filter
import relaks.lang.dsl.extensions.ast._
import relaks.lang.dsl.extensions.{SQLCompilers, DrillCompilers, TableCompilerPhases, TableExtensions}
import shapeless._

/**
 * Created by Pietras on 22/05/15.
 */
class TableTest extends FunSpec with Matchers with Inside with LoneElement with Inspectors with LazyLogging {

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

        res.tree should matchPattern { case _/> Filter(_, _, _/> Apply(Stdlib.==, _)) => }
      }

      it("should construct a filter expression from a for comprehension") {
        object Program extends DSL with TableExtensions
        import Program._
        val a = load("hello")
        val res = for {
          as: Row2[Int, Int] <- a(('x, 'y)) if as(0) === 1
        } yield (as(0), 1)

        analyze(res.tree)

        res.tree should matchPattern { case _/> Transform(_, _/> Filter(_, _, _/> Apply(Stdlib.==, _)), _) => }
      }

    }
    describe("ast rewriter") {
      it("should unnest nested comprehensions into joins") {
        object Program extends DSL with TableExtensions with TableCompilerPhases
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

//        analyze(res.tree)
        val transformed = fuseTransforms(res.tree).get.asInstanceOf[TTree]
//        analyze(transformed)

        transformed should matchPattern { case _/>Transform(_, _/>Join(_, (_ ,_/>Join(_, _, CartesianJoin, _)), CartesianJoin, _), _/>(_: Pure)) => }
      }

      it("should unnest nested comprehensions with filters into non-cartesian joins") {
        object Program extends DSL with TableExtensions with TableCompilerPhases
        import Program._
        import org.kiama.rewriting.Rewriter._

        val a = load("hello")
        val b = load("world")

        val res = for {
          as: Row2[Int, Int] <- a(('ax, 'ay))
          bs: Row2[Int, Int] <- b(('bx, 'by)) if as(0) === bs(0)
        } yield (as(1), bs(1))

        analyze(res.tree)
        val transformed = fuseTransforms(res.tree).get
        transformed should matchPattern { case _/>Transform(_, _/>Join(_, _, InnerJoin, Some((_, _/>Apply(Stdlib.==, _)))), _/>(_: Pure)) => }
      }

      it("should unnest multiple comprehensions with filters into non-cartesian joins") {
        object Program extends DSL with TableExtensions with TableCompilerPhases
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

        analyze(res.tree)
        val transformed = fuseTransforms(res.tree).get
        transformed should matchPattern { case _/>Transform(_, _/>Join(_, (_, _/>Join(_, _, InnerJoin, _)), InnerJoin, _), _/>(_: Pure)) => }
      }
      it("should generate projections") {
        object Program extends DSL with TableExtensions with TableCompilerPhases with DrillCompilers
        import Program._
        val a = load("hello")
        val b = load("world")

        val res = for {
          as: Row2[Int, Int] <- a(('ax, 'ay))
          bs: Row2[Int, Int] <- b(('ax, 'by))
        } yield (as(0), as(1), bs(0), bs(1))


        val transformed = fuseTransforms(res.tree).get
        println(transformed.verboseString)

        def collectGenerators(expr: Expression): Seq[Generator] = {
          val sources = Sources.unapply(expr)
          val generator = QueryWithGenerator.unapply(expr).map(_._1).map(Seq(_)).getOrElse(Seq.empty[Generator])
          sources.getOrElse(Seq.empty[Atom]).foldLeft(generator)((seq, source) => seq ++ collectGenerators(source))
        }

        val fields = collectGenerators(transformed).map(_.symsToFields.values)

        forAll (fields) { vals: Iterable[Symbol] => (vals.toSet.size) should equal (vals.size) }

        inside (transformed) {
          case _/>Transform(_, _/>Join(_, (_, _/> Transform(gen: Generator, _, expr)), _, _), _) =>
            //takes 'ax...
            gen.symsToFields.values.loneElement should equal ('ax)
            inside(expr) {
              //and transforms it to 'ax__*
              case _/>Pure(_/>(t: TupleConstructor)) => atLeast(1, t.names) should startWith ("ax__")
            }
        }
      }
    }
    describe("drill compiler") {
      it("should compile a load table expression") {
        object Program extends TableExtensions with TableCompilerPhases with DrillCompilers

        val table = Program.load("hello")
        val compiler = new Program.CompileDrill(Map.empty)
        val result = compiler(table.tree).written.asInstanceOf[Map[Program.Sym, LogicalOperator]] //cast needed by lone element
        result.loneElement should matchPattern { case (_, _: Scan) => }
      }

      it("should compile a simple transform") {
        object Program extends DSL with TableExtensions with TableCompilerPhases with DrillCompilers
        import Program._

        val a = Program.load("hello")
        val res = for {
          as: Row2[Int, Int] <- a(('ax, 'ay))
        } yield (as(0), as(1))

        val compiler = new CompileDrill(Map.empty)
        val result = compiler(res.tree).written

        result.values.toSeq should have length 2
        result.values.exists(_.isInstanceOf[DrillTransform]) should be(true)
        result.values.exists(_.isInstanceOf[Scan]) should be(true)
      }

      it("should compile a join") {
        object Program extends DSL with TableExtensions with TableCompilerPhases with DrillCompilers
        import Program._
        val a = load("hello")
        val b = load("world")

        val res = for {
          as: Row2[Int, Int] <- a(('ax, 'ay))
          bs: Row2[Int, Int] <- b(('bx, 'by))
        } yield (as(0), as(1), bs(0), bs(1))

        val projected = fuseTransforms(res.tree).get
        val compiler = new CompileDrill(Map.empty)
        val result = compiler(projected).written

        result.values.exists(_.isInstanceOf[DrillJoin]) should be(true)
      }

    }

    describe("sql compiler") {
      it("should compile a join") {
        object Program extends DSL with TableExtensions with TableCompilerPhases with SQLCompilers
        import Program._
        val a = load("hello")
        val b = load("world")

        val res = for {
          as: Row2[Int, Int] <- a(('ax, 'ay))
          bs: Row2[Int, Int] <- b(('ax, 'ay))
        } yield (as(0), as(1), bs(0), bs(1))

//        val (fields, projected) = CompileRelational.analyzeRewriteTree(res.tree).toOption.get.run

        val compiler = new CompileSQL(Map.empty)
        val result = compiler(fuseTransforms(res.tree).get).value

        println(result)
      }

    }
  }
}
