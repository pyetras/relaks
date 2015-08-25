package relaks.lang.dsl

import org.scalatest._
import relaks.lang.ast._
import relaks.lang.dsl.extensions.ListInterpreter
import relaks.lang.dsl.extensions.ast.logical.{LoadComprehension, SelectComprehension}
import relaks.lang.phases.interpreter.ListComprehensionInterpreter
import shapeless.{HNil, ::}

import scalaz.EphemeralStream

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
//    it("should allow non-lifted types") {
//      List(true, 1).getTpe shouldEqual (listType[AnyVal])
//    }
    describe("interpreter") {
      it("should interpret a list constructor expression") {
        object Program extends DSL with ListInterpreter
        import Program._
        val stream = evalExpression(List(1, 2, 3).tree).asInstanceOf[EphemeralStream[Int]]
        stream.toList should equal(scala.List(1,2,3))
      }

      it("should interpret explicitly converted list") {
        object Program extends DSL with ListInterpreter
        import Program._

        val l: Rep[List[Int]] = scala.List(4, 5, 6)
        val stream = evalExpression(l.tree).asInstanceOf[EphemeralStream[Int]]
        stream.toList should equal(scala.List(4, 5, 6))
      }

      it("should transform a list comprehension expression") {
        object Program extends ListComprehensionInterpreter with DSLInterpreter
        import Program._

        val l = List(1, 2, 3, 4, 5, 6)
        val tab = l.asTable

        tab.tree should matchPattern { case _/> Transform(_, _/> TableFromList(_), _/> Pure(_)) => }

        val cmp = buildComprehensions(tab.tree).get

        cmp should matchPattern { case _/> SelectComprehension(LoadComprehension(TableFromList(_)), _ +: Seq(), Seq(), Seq(), Seq(), _) => }
      }

      it("should interpret a list to comprehension expression") {
        object Program extends ListComprehensionInterpreter with DSLInterpreter
        import Program._

        val l = List(1, 2, 3, 4, 5, 6)
        val tab: Rep[TypedTable[Tup[Int::HNil]]] = l.asTable
        val Some((rows, err)) = Program.run(buildComprehensions(tab.tree).get)
        rows.map(_(0)) should equal (scala.List(1, 2, 3, 4, 5, 6))
      }

      it("should compute average") {
        object Program extends ListComprehensionInterpreter with DSLInterpreter
        import Program._

        val l = List(1, 2, 3, 4, 5)
        val avg: Rep[Double] = l.asTable.avg
        val d = Program.evalExpression(buildComprehensions(avg.tree).get)
        d should equal(3.0)
      }

      it("should interpret a list of tuples to comprehension expression") {
        pending
      }
    }
  }

}