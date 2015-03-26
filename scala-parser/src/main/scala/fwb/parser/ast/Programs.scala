package fwb.parser.ast
import scalaz.{Tree => SZTree}

/**
 * Created by Pietras on 22/03/15.
 */
object Programs {
  implicit class Program(val children: Traversable[Tree]) extends Tree
  object Program {
    def unapply(program: Program) = Some(program.children)
  }
}
