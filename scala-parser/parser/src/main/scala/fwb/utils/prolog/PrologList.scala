package fwb.utils.prolog

import jpl.Term

import scala.annotation.tailrec

/**
 * Created by Pietras on 22/03/15.
 */

object PrologList {

  implicit class PrologList(term: Term) extends Traversable[Term] {
//    implicit def Term2PrologList[U](term: Term): PrologList = new PrologList(term)

    def foreach[U](f: Term => U) = {
      @tailrec
      def _iterate(term: Term, sep: Option[String]): Unit = {
        for (i <- 0 until (term.arity() - 1))
          f(term.args()(i))

        if (term.arity() > 1 && term.isCompound()) {
          val next = term.args()(term.arity() - 1)
          if (next.isCompound()) {
            sep match {
              case Some(name) if name == next.name() => _iterate(next, Some(name))
              case None if Array(",", ".", term.name()).contains(next.name()) => _iterate(next, Some(term.name()))
              case _ => {
                if (next.name() != "[]") f(next) else ()
              }
            }
          } else f(next)
        }
      }
      _iterate(term, None)
    }
  }

}
