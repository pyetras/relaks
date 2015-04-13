package fwb.dsl

import fwb.dsl.AST._

/**
 * Created by Pietras on 10/04/15.
 */
trait Computation[T] {
  type TTree = Expression
  def tree: TTree
  override def toString = s"Computation($tree)"
}


