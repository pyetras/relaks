package fwb.parser.parsers

import fwb.parser.ast.Programs.Program

/**
 * Created by Pietras on 24/03/15.
 */
trait FWBParser[T] {
  def parse(terms: T): Program
}
