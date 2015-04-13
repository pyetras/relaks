package fwb.dsl.flow

import scala.language.experimental.macros
import scala.language.implicitConversions

/**
 * Created by Pietras on 29/03/15.
 */
object Flow {
  import shapeless._
  import ops.function._
  import ops.record._
  import syntax.std.function._

  import scalax.collection.edge.Implicits._

  case class EdgeIO[L, R, W](node: Node[L, R], name: W)
  abstract class Node[L, R]{
    def call(args: L): R

    def apply(value: Witness) = EdgeIO[L, R, value.T](this, value.value)
  }

  implicit class LeftNode[L, R, W](edgeio: EdgeIO[L, R, W]) {
    val node: Node[L, R] = edgeio.node
    val retName: W = edgeio.name

    def ~>[L2, R2, W2](right: RightNode[L2, R2, W2]) =
      (this.node ~+> right.node)(EdgeLabel(this.retName, right.argName))

  }
  case class RightNode[L, R, W](node: Node[L, R], argName: W)

  case class EdgeLabel[W1, W2](w1: W1, w2: W2)

//  implicit def edgeIOtoLeftNode[L, R <: HList, W](edgeio: EdgeIO[L, R, W])(implicit sel: Selector[R, W]): LeftNode[L, R, W] =
//    LeftNode[L, R, W](edgeio.node, edgeio.name)

  implicit def edgeIOtoRightNode[L <: HList, R, W](edgeio: EdgeIO[L, R, W])(implicit sel: Selector[L, W]): RightNode[L, R, W] =
    RightNode(edgeio.node, edgeio.name)


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
