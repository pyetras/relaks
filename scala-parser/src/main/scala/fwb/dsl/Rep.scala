package fwb.dsl

import fwb.dsl.AST._

/**
 * Created by Pietras on 10/04/15.
 */
trait Rep[+T] {
  def tree: TTree
  override def toString = s"Rep($tree)"
}


