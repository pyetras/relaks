package relaks.lang.dsl.flow

import org.scalatest._

/**
 * Created by Pietras on 31/03/15.
 */
class CallApplyTest extends FunSpec with Matchers {
  describe("CallApply macro") {
    case class Test(arg1: Int, arg2: String) {
      def apply() = Map(arg2 -> arg1)
    }

    type RetType = Map[String, Int]

    it("should materialize a case class applicator") {
      val app = implicitly[CallApply[Test]]
      val inst = Test(1, "hello")
      app(inst) shouldBe a[RetType]
    }

    it("should infer correct return argument") {
      """implicitly[CallApply.Aux[Test, RetType]]""" should compile
    }

    it("should not compile when a class does not have an apply") {
      case class Test2(arg1: Int)
      """implicitly[CallApply[Test2]]""" shouldNot compile
    }

    it("should not compile when the apply method takes an argument") {
      case class Test3(arg1: Int) {
        def apply(arg2: Int) = arg1 + arg2
      }
      """implicitly[CallApply[Test3]]""" shouldNot compile
    }

    it("should work for a class without constructor arguments") {
      class Test4 {
        def apply() = "hello"
      }
      """implicitly[CallApply.Aux[Test4, String]]""" should compile
    }
  }
}
