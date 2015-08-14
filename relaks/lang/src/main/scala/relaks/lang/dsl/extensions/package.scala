package relaks.lang.dsl

/**
 * Created by Pietras on 15.08.15.
 */
package object extensions {
  implicit class VectorWithMapFS[A, B](val s: Vector[(A, B)]) extends AnyVal {
    def mapFirst[C](f: A => C) = s.map { case (a, b) => f}
  }
}

