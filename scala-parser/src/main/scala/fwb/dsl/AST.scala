package fwb.dsl

import fwb.ast._

/**
 * Created by Pietras on 13/04/15.
 */
object AST extends ASTNodes {
  type TTree = Expression
  private[dsl] trait ASTSyntax extends ConstantOps with ScalaTypeImplis
  object syntax extends ASTSyntax {}
}
