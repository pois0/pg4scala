package jp.pois.pg4scala
package utils

import utils.Character.UnicodeRange

private[utils] trait IntMap[+V] extends Map[Int, V]{
  override def +[V1 >: V](kv: (Int, V1)): Map[Int, V1] = {
    val builder = Map.newBuilder[Int, V1]
    for (e <- iterator) builder += e
    builder.result() + kv
  }

  override def iterator: Iterator[(Int, V)] = UnicodeRange.flatMap { e => get(e).map { v => (e, v) } }.iterator

  override def -(key: Int): Map[Int, V] = {
    val builder = Map.newBuilder[Int, V]
    for (e <- iterator) builder += e
    builder.result() - key
  }

  private def generalMap = {
    val builder = Map.newBuilder[Int, V]
    for (e <- iterator) builder += e
    builder.result()
  }
}
