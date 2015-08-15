package relaks.lang.dsl

/**
 * Created by Pietras on 11/04/15.
 */

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import relaks.lang.ast.{ScalaNumType, ArgType, ScalaType}
import breeze.math.{Field => FieldZ}
import scalaz.{Ordering, Order}
import scalaz.Scalaz._
import scalaz.syntax.{ToOrderOps, OrderOps, OrderSyntax}
import relaks.lang.ast.Literal

class OperationsTest extends FunSpec with Matchers with Inside with Checkers with ToOrderOps {
  describe("Interpreter") {
    describe("for numeric ops") {
      it("should interpret binary operations") {
        object Program extends DSLInterpreter
        import Program._
        def checker[T: ScalaNumType: FieldZ](a: T, b: T)(implicit f: FieldZ[T]) = {
          val ra: Rep[T] = a
          val rb: Rep[T] = b
          val result = (res: Rep[T]) => evalExpression(res.tree)
          result(ra + rb) == f.+(a, b) &&
          result(ra - rb) == f.-(a, b) &&
          result(ra * rb) == f.*(a, b) &&
          ((b == 0) ? true | result(ra / rb) == f./(a, b))
        }
        check(checker[Int] _)
        check(checker[Double] _)
        check(checker[Long] _)
      }
    }
    describe("for orderable ops") {
      it("should interpet staged orderable operations") {
        object Program extends DSLInterpreter
        import Program._
        def checker[T : ScalaType: Order](a: T, b: T)(implicit ev: T => Rep[T] = (x: Any) => x.asRep.asInstanceOf[Rep[T]]) = {
          val ra: Rep[T] = a
          val rb: Rep[T] = b
          val result = (res: Rep[Boolean]) => evalExpression(res.tree)
          result(ra < rb) == a < b &&
          result(ra <= rb) == a <= b  &&
          result(ra >= rb) == a >= b &&
          result(ra > rb) == a > b
        }
        check(checker[Int] _)
        check(checker[Double] _)
        check(checker[Long] _)
        check(checker[String] _)
      }

      it("should interpret native orderable operations") {
        object Program extends DSLInterpreter
        import Program._

        case class A(i: Int)
        implicit val aOrder = new Order[A] {
          override def order(x: A, y: A): Ordering = implicitly[Order[Int]].order(x.i, y.i)
        }

        val ra = A(1).asRep
        val rb = A(10).asRep

        evalExpression((ra < rb).tree) should equal(true)
        evalExpression((ra >= rb).tree) should equal(false)
      }
    }
  }

}
