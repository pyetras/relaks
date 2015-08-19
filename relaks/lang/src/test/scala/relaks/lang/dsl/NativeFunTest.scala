package relaks.lang.dsl

import org.scalatest.{Inside, Matchers, FunSpec}
import relaks.lang.ast._
import relaks.lang.dsl.extensions.TupleInterpreter
import relaks.lang.impl.{UntypedTableImpl, TableImpl}
import relaks.lang.impl
import relaks.lang.phases.interpreter.NativeInterpreter

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

      val z = to(f _) apply Tuple1((new B).asRep)
      z.tree should matchPattern { case ApplyNative(f, _) => }
    }

    it("should translate Table type") {
      object Program extends DSL

      def f(a: UntypedTableImpl) = null
      """import Program._; to (f _) apply Tuple1(null.asInstanceOf[UntypedTable].asRep)""" should compile
    }

    describe("evaluation") {
      it("should evaluate a simple function with a staged result type") {
        object Program extends DSL with TupleInterpreter with NativeInterpreter
        import Program._

        def f(a: Int, b: Int) = a + b + 1
        val c = to (f _) apply (3, 5)
        evalExpression(c.tree) should equal(9)
      }

      it("should evaluate a function to a non-staged type") {
        object Program extends DSL with TupleInterpreter with NativeInterpreter
        import Program._

        def f(r: impl.Row) = Set(r.values:_*)
        val row: Rep[Tup[_]] = (1, 2, 3, 4, 5.0)
        val s = to (f _) apply Tuple1(row)

        evalExpression(s.tree).asInstanceOf[Set[Any]] should contain allOf(1, 2, 3, 4, 5.0)
      }
    }
  }
}
