package relaks.lang.dsl

import org.scalatest._
import relaks.lang.ast._

/**
 * Created by Pietras on 17/04/15.
 */
class TupleTest extends FunSpec with Matchers with Inside with LoneElement {
  import AST._
  object Program extends DSL
  import Program._

  import scala.collection.immutable.{List => SList}

  describe("Staged tuple") {
    import shapeless._ // FIXME: leo why
    val x = choose between 1 and 3
    val t = (1, 4: Rep[Int], x, true)
    type TType = Int::Int::Int::Boolean::HNil
    val tup: Rep[Tup[TType]] = t

    it("should create correct element types") {
      val typ = tup.getTpe.asInstanceOf[TupType[TType]]
      typ.childrenTypes should have length 4
    }

//    it("should superpos type if necessary") {
//      tup.getTpe.isSuperPosed should be(true)
//      (1, 4: Rep[Int], true).getTpe.isSuperPosed should be(false)
//    }

    it("should allow static access") {
      tup(0).tree should matchPattern { case Literal(1) => }
      tup(0).getTpe should equal(intType)
//      tup(0).getTpe.isSuperPosed should be(false)

//      tup(2).getTpe.isSuperPosed should be(true)
//      inside(tup(2).getTpe) { case t:SuperPosArgType[_] => t.insideType should equal(intType) }
    }

    it("should allow dynamic access") {
      tup.at(0).tree should matchPattern { case Apply(Stdlib.at, x) => }
      val i: Rep[Int] = 0
//      tup.at(i).getTpe.isSuperPosed should be(true)
    }

    it("should not create a tuple with incorrect type") {
      """(1, 2, Set[Int]): Rep[_]""" shouldNot compile
    }

    it("should support unpacking") {
      val Tup((x, y)) = tupleToRep((1, true))
      x shouldBe an [Rep[_]]
      """x * 1""" should compile
      y shouldBe a [Rep[_]]
      y.getTpe should equal(boolType)
      """y * 1""" shouldNot compile
    }

    describe("with labels") {
      it("should be created from single element with `as` syntax") {
        val t1: Rep[Tup[Int::HNil]] = 1 as "hello"
        t1.tree.asInstanceOf[TupleConstructor].names.loneElement should equal("hello")
        t1.tree.asInstanceOf[TupleConstructor].tuple should have length(1)
      }

      it("should be created from a tuple of elements with `as` syntax") {
        val tup = (1 as "text", "string" as "int")
        val t2: Rep[Tup[Int::String::HNil]] = tup
        t2.tree.asInstanceOf[TupleConstructor].names should be (Vector("text", "int"))
        t2.tree.asInstanceOf[TupleConstructor].tuple should have length(2)
      }
    }

  }

}