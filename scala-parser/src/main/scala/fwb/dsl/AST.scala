package fwb.dsl

import fwb.ast._

/**
 * Created by Pietras on 13/04/15.
 */
object AST extends ASTNodes {
  private[dsl] trait ASTSyntax extends ConstantOps with ScalaTypeImplis
  object syntax extends ASTSyntax {}
}
