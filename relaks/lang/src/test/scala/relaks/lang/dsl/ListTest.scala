package relaks.lang.dsl

import org.scalatest._

/**
 * Created by Pietras on 16/04/15.
 */
class ListTest extends FunSpec with Matchers with Inside {
  object Program extends DSL
  import Program._

  import scala.collection.immutable.{List => SList}

  describe("Staged lists") {
    it("should be built with correct types") {

      List(1, 2, 3).tree.tpe should equal (listType[Int])
      List[Int]().tree.tpe should equal (listType[Int])
      List(1, 2, 3) shouldBe a[Rep[_]]

      List(true, false).tree.tpe should equal (listType[Boolean])

      listToRep(SList(1.0d, 2.0d)).tree.tpe should equal (listType[Double])

      val superPosed = choose between 1 and 10
      List(superPosed).tree.tpe should equal (listType[Int]) //FIXME

      List(List(1, 2)).tree.tpe should equal (listType[List[Int]])
    }
    it("should implicitly convert scala lists") {
      """implicitly[List[Boolean] => Rep[List[Boolean]]]""" should compile
      """implicitly[List[Int] => Rep[List[Int]]]""" should compile
      """implicitly[List[Double] => Rep[List[Double]]]""" should compile
      """implicitly[List[String] => Rep[List[String]]]""" should compile
      """implicitly[List[Long] => Rep[List[Long]]]""" should compile
    }
    it("should allow non-lifted types") {
      List(true, 1).getTpe shouldEqual (listType[AnyVal])
    }
    describe("evaluation") { pending }
  }

}