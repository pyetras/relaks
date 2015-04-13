package fwb.dsl

/**
 * Created by Pietras on 11/04/15.
 */

import fwb.dsl.AST._
import fwb.ast.Constants.Constant
import org.scalatest._
import fwb.dsl.syntax._

class OperationsTest extends FunSpec with Matchers with Inside {
  describe("Computation extensions") {
    describe("should build a tree for") {
      it("comparison operators") {
        val x = choose between 5 and 10
        val y = x < 5
        inside(y.tree) { case Apply(Operator("<"), lst: List[Expression]) =>
          lst should matchPattern { case hd :: tl if tl == List(Literal(Constant(5))) => } }

      }
    }
  }

}
