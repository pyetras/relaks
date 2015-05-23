package relaks.lang.dsl

import shapeless.HList
import shapeless.ops.hlist.ToTraversable

/**
 * Created by Pietras on 23/05/15.
 */
package object utils {
  type TupleLU[H <: HList, LU] = ToTraversable.Aux[H, List, LU] //TODO: make it more efficient and preserve classtag
}
