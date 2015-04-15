package fwb.dsl.ops

import fwb.dsl._
import AST._

import scala.language.implicitConversions

/**
 * Created by Pietras on 15/04/15.
 */
trait AnyOps {
  implicit def anyToRep[B1](x: B1)(implicit tpe: ScalaType[B1]): Rep[ScalaType[B1]] = new Rep[ScalaType[B1]] {
    override def tree: TTree = Literal(x)
  }
}
