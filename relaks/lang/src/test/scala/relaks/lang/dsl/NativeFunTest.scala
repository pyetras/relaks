package relaks.lang.dsl

import org.scalatest.{Inside, Matchers, FunSpec}
import relaks.lang.ast.{ScalaTypes, TupleConstructor, ApplyNative}

/**
 * Created by Pietras on 23/06/15.
 */
class NativeFunTest extends FunSpec with Matchers with Inside {

  def branin(x: Double, y: Double) = {
    val a = x*15
    val b = y*15-5
    val sq: Double => Double = math.pow(_, 2)

    sq(b - (5.1/(4*sq(math.Pi)))*sq(a) + (5/math.Pi)*a - 6) + 10*(1-(1./(8*math.Pi)))*math.cos(a) + 10
  }

  describe("Native Function Extensions") {
    it("should construct a node for a native function call") {
      object Program extends DSL
      import Program._

      val x = choose from List(1.0, 3.0, 4.0)
      val y = choose from List(1.0, 3.0, 4.0)
      val a = x + y
      val z = to (branin _) apply (x, a)

      z.tree should matchPattern  {case ApplyNative(branin, _/>(_: TupleConstructor)) => }
      z.getTpe should equal (ScalaTypes.doubleType) //test if it doesn't return NativeType (unlifted) by any chance
    }
  }
}
