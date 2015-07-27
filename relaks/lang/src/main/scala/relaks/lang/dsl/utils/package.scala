package relaks.lang.dsl

import shapeless.HList
import shapeless.ops.hlist.ToTraversable

/**
 * Created by Pietras on 23/05/15.
 */
package object utils {
  type TupleLU[H <: HList, LU] = ToTraversable.Aux[H, List, LU] //TODO: make it more efficient and preserve classtag

  trait NamesToStrings[H <: HList] {
    def apply(h: H): Vector[String]
  }

  object NamesToStrings {
    def apply[H <: HList](implicit names: NamesToStrings[H]) = names
    implicit def symbolNamesToStrings[H <: HList](implicit namesT: ToTraversable.Aux[H, Vector, Symbol]): NamesToStrings[H] =
      new NamesToStrings[H] {
        override def apply(h: H): Vector[String] = namesT.apply(h).map(_.name)
      }

    implicit def stringNamesToString[H <: HList](implicit namesT: ToTraversable.Aux[H, Vector, String]): NamesToStrings[H] =
      new NamesToStrings[H] {
        override def apply(h: H): Vector[String] = namesT.apply(h)
      }
  }
}
