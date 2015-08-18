package relaks.lang.dsl

import org.scalatest.{Inside, Matchers, FunSpec}
import relaks.lang.ast.Table
import relaks.lang.impl.TableImpl
import scalikejdbc.{ConnectionPool, WrappedResultSet}
import relaks.lang.impl
import scalaz._

/**
 * Created by Pietras on 16.08.15.
 */
class DrillInterpreterTest extends FunSpec with Matchers with Inside {
  describe("Drill interpreter") {
    describe("rows process") {
      it("should fetch rows from a csv file", org.scalatest.Tag("relaks.Integration")) {
        object Program extends DSLDrillInterpreter
        import Program._
        val a = load("/Users/Pietras/Downloads/train-1.csv").apply(Symbol("columns[0]").::[String], Symbol("columns[1]").::[Int]) filter { row =>
          row(1) > 0
        } map { row =>
          (row(0) as 'hello, (row(1) * 10) as 'world)
        } limit 10
        val proc = evalComprehension(ComprehensionBuilder.comprehension(a.tree).get)
        val buffer = collection.mutable.Buffer.empty[impl.Row]
        proc.to(stream.io.fillBuffer(buffer)).run.run
        Program.show(buffer, None)
        buffer should have length 10

      }
    }
    describe("table impl") {
      it("should evaluate a comprehenion as a tableImpl instance", org.scalatest.Tag("relaks.Integration")) {
        object Program extends DSLDrillInterpreter
        import Program._
        val a = load("/Users/Pietras/Downloads/train-1.csv").apply(Symbol("columns[0]").::[String], Symbol("columns[1]").::[Int]) filter { row =>
          row(1) > 0
        } map { row =>
          (row(0) as 'hello, (row(1) * 10) as 'world)
        } limit 10

        val buffer = collection.mutable.Buffer.empty[impl.Row]
        def f(tbl: TableImpl) =
          buffer ++= tbl.toIterator

        val b = to(f _) apply tupleToRep(Tuple1(a.asInstanceOf[Rep[Table]]))
        val result = evalExpression(buildComprehensions(b.tree).get)

        result should equal(buffer)
        buffer should have length 10
      }
    }
  }
}
