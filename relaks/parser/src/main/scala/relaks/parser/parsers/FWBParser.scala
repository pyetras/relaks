package relaks.parser.parsers

import relaks.parser.AST

/**
 * Created by Pietras on 24/03/15.
 */
trait FWBParser[T] {
  import AST._
  def parse(terms: T): Program
}
