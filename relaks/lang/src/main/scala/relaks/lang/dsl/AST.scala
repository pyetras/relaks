package relaks.lang.dsl

import relaks.lang.ast._

/**
 * Created by Pietras on 13/04/15.
 */
object AST extends Stdlib {
  type TTree = Expression
  private[dsl] trait ASTSyntax extends ScalaTypeImplis with ToTypedTreeOps
  object syntax extends ASTSyntax {}
}
