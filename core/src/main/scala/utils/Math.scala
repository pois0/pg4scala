package jp.pois.pg4scala
package utils

private[pg4scala] object Math {
  @inline
  def maxBy[A, B](f: A => B)(e1: A, e2: A)(implicit cmp: Ordering[B]): A = if (cmp.gt(f(e2), f(e1))) e2 else e1

  @inline
  def minBy[A, B](f: A => B)(e1: A, e2: A)(implicit cmp: Ordering[B]): A = if (cmp.lt(f(e2), f(e1))) e2 else e1
}
