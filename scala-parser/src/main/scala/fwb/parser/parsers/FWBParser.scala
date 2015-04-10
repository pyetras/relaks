package fwb.parser.parsers

import fwb.ast.AST

/**
 * Created by Pietras on 24/03/15.
 */
trait FWBParser[T] {
  import AST._
  def parse(terms: T): Program
}
