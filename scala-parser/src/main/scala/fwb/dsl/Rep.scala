package fwb.dsl

import fwb.dsl.AST._

/**
 * Created by Pietras on 10/04/15.
 */
abstract class Rep[+T] {
  def tree: TTree
  def getTpe = tree.tpe
  override def toString = s"Rep($tree)"
}


