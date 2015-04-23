package relaks.dsl

import relaks.dsl.AST._

/**
 * Created by Pietras on 10/04/15.
 */

//for instance Rep[Int], Rep[List[Int]]
trait Rep[+T] {
  val tree: Expression
  def getTpe = tree.tpe
  override def toString = s"Rep($tree)"
}


