package fwb.dsl

import org.scalatest._

/**
 * Created by Pietras on 17/04/15.
 */
class ProductTest  extends FunSpec with Matchers with Inside {
  import AST._
  object Program extends DSL
  import Program._
  import scala.collection.immutable.{List => SList}

  describe("Staged products") {
    it("test suite") {
      import shapeless._ // FIXME: leo why
      val x = choose between 1 and 3
      val t = (1, 4: Rep[Int], x)
      (productToRep(t):Rep[_]).toString
    }
  }

}