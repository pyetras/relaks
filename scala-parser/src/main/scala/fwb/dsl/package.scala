package fwb

import fwb.ast._
import fwb.dsl.AST._

/**
 * Created by Pietras on 11/04/15.
 */
package object dsl {
  object syntax extends OperationsSyntax with SuperPosMapperImplis with ASTSyntax with SuperPosOps {

  }
}
