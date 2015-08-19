package relaks.lang.dsl

import org.scalatest.{Inside, Matchers, FunSpec}
import relaks.lang.ast.{Tup, TypedTable, Table}
import relaks.lang.impl.{TypedTableImpl, TableImpl}
import scalikejdbc.{ConnectionPool, WrappedResultSet}
import relaks.lang.impl
import shapeless._
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
        val aa = load("/Users/Pietras/Downloads/train-1.csv").apply(Symbol("columns[0]").::[String], Symbol("columns[1]").::[Int])
        val a = aa filter { row =>
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
        def f(tbl: TypedTableImpl[String::Int::HNil]) = {
          buffer ++= tbl.toIterator
        }

        val b : CallWord[collection.mutable.Buffer[impl.Row], TypedTable[Tup[String :: Int :: HNil]] :: HNil] = to(f _) //apply tupleToRep(Tuple1(a))
        val c = (f _).pure.apply(Tuple1(a))
        val result = evalExpression(buildComprehensions(c.tree).get)

        result should equal(buffer)
        buffer should have length 10
      }
    }
  }
}
