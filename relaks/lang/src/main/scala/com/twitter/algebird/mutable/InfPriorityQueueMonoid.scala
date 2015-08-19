package com.twitter.algebird.mutable

import java.util.PriorityQueue
import scalaz.Scalaz._

/**
 * Created by Pietras on 19.08.15.
 */
class InfPriorityQueueMonoid[K](implicit ord: Ordering[K]) extends PriorityQueueMonoid[K](1) {
  override def build(items: Iterable[K]): PriorityQueue[K] = {
    val pq = new PriorityQueue[K](items.size, ord.reverse)
    items.foreach(pq.add)
    pq
  }

  override protected def limit(q: PriorityQueue[K]): Unit = throw new NotImplementedError()

  override def plus(left: PriorityQueue[K], right: PriorityQueue[K]): PriorityQueue[K] = {
    val (bigger, smaller) = (left.size() > right.size()) ? (left, right) | (right, left)
    bigger.addAll(smaller)
    bigger
  }
}
