package fwb

import fwb.ast._
import fwb.dsl.AST._

/**
 * Created by Pietras on 11/04/15.
 */
package object dsl {
  object syntax extends OperationsSyntax with SupPosMapperImplis with ASTSyntax {
    object choose extends ToTypedTreeOps {
      trait Between[T] {
        val from: T
        def and(t: T)(implicit ev: T => Value, typ: ScalaType[T]) : Computation[SupPos[T]] = new Computation[SupPos[T]] {
          override def tree: TTree = new Nondet(ChoiceRange(from, t))(typ.supPosType)
        }
      }
      def between[T](frm: T) = new Between[T] { val from = frm }
      def from[T](from: Seq[T])(implicit ev: Seq[T] => Lst, typ: ScalaType[T]) : Computation[SupPos[T]] = new Computation[SupPos[T]] {
        override def tree: TTree = new Nondet(ChoiceList(from))(typ.supPosType)
      }
    }

  }
}
