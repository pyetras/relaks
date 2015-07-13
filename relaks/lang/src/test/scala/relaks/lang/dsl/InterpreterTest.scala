package relaks.lang.dsl

import org.scalatest.{Matchers, Inside, FunSpec}

import scalaz.stream.process1

/**
 * Created by Pietras on 12/07/15.
 */
class InterpreterTest extends FunSpec with Matchers with Inside {
  describe("interpreter") {
    it("should interpret a simple expression") {
      object Program extends DSLInterpreter
      import Program._
      val b = choose between 1 and 3
      val r = (optimize (Tuple1(b)) map { row =>
        Tuple1(row(0))
      }) orderBy Tuple1('x0)


      val stream = eval(buildComprehensions(r.tree).get)
      stream.runLog.run should have length 2
    }

    it("should print a stored table") {
      object Program extends DSLInterpreter {
        val b = choose between 1 and 3
        val r = (optimize(Tuple1(b)) map { row =>
          Tuple1(row(0))
        }) orderBy Tuple1('x0)

        store(r)
      }

      Program.dump()
    }

  }

}
