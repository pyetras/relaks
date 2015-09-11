package org.kiama.relation

import relaks.lang.ast.Expression

/**
 * Created by Pietras on 23/06/15.
 */

//a graph tree is not a tree but a dag lol
//TODO rename this to ASDag?
class DAGIr(val root_ : Expression) extends Tree[Expression, Expression](root_) {
  override lazy val child: TreeRelation[Expression, Expression] = new TreeRelation(childGraph)
}
