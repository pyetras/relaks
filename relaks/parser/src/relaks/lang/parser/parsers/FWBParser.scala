package relaks.lang.parser.parsers

import relaks.lang.parser.AST

/**
 * Created by Pietras on 24/03/15.
 */
trait FWBParser[T] {
  import AST._
  def parse(terms: T): Program
}
