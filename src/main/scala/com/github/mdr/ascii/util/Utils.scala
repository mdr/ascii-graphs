package com.github.mdr.ascii.util

object Utils {

  def transformValues[K, V, V2](map: Map[K, V])(f: V ⇒ V2): Map[K, V2] = map.map { case (k, v) ⇒ (k, f(v)) }

  def withPreviousAndNext[T](iterable: Iterable[T]): List[(Option[T], T, Option[T])] = {
    if (iterable.isEmpty)
      Nil
    else {
      val previous = None :: (iterable.init map Some[T]).toList
      val next = (iterable.tail map Some[T]).toList ::: List(None)
      previous zip iterable zip next map { case ((a, b), c) ⇒ (a, b, c) }
    }
  }

}