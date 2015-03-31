package fwb.api

import shapeless.ops.function.FnToProduct
import scala.language.experimental.macros


/**
 * Created by Pietras on 29/03/15.
 */
object Flow {
  import shapeless._
  import syntax.std.function._
  import ops.function
  import syntax.singleton._
  import record._


  abstract class Node[L, R]{
    def call(args: L): R

  }

  object Node {
    def apply[L](implicit lgen: LabelledGeneric[L], ev: CallApply[L]): Node[lgen.Repr, ev.Ret] = {
      new Node[lgen.Repr, ev.Ret] {
        def call(args: lgen.Repr): ev.Ret = {
          ev(lgen.from(args))
        }
      }
    }

    def apply[F, L <: HList, R](f: F)(implicit fp: FnToProduct.Aux[F, L => R]): Node[L, R] = {
      new Node[L, R] {
        def call(args: L): R = {
          f.toProduct(args)
        }
      }
    }
  }
}
