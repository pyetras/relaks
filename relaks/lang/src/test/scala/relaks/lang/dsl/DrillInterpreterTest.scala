package relaks.lang.dsl

import org.scalatest.{Inside, Matchers, FunSpec}
import scalikejdbc.{ConnectionPool, WrappedResultSet}
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
        val a = load("/Users/Pietras/Downloads/sampleSubmission.csv")
        val proc = Program.rowsProcess(ComprehensionBuilder.comprehension(a.tree).get)
        val buffer = collection.mutable.Buffer.empty[Map[String, Any]]
        proc.map(_.toMap()).to(stream.io.fillBuffer(buffer)).run.run
        println(buffer)
      }
    }
  }
}
