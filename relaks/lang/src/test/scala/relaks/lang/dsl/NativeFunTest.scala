package relaks.lang.dsl

import org.scalatest.{Inside, Matchers, FunSpec}
import relaks.lang.ast.{ScalaTypes, TupleConstructor, ApplyNative, Table}
import relaks.lang.impl.TableImpl

/**
 * Created by Pietras on 23/06/15.
 */
class NativeFunTest extends FunSpec with Matchers with Inside {
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

    it("should construct a covariant call") {
      object Program extends DSL
      import Program._

      class A
      class B extends A

      def f(a: A) = null
      val z = to (f _) apply Tuple1((new B).asRep)
      z.tree should matchPattern { case ApplyNative(f, _ ) => }
    }

    it("should translate Table type") {
      object Program extends DSL

      def f(a: TableImpl) = null
      """import Program._; to (f _) apply Tuple1(null.asInstanceOf[Table].asRep)""" should compile
    }
  }
}
