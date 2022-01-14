package jp.pois.pg4scala
package utils

import utils.Character.UnicodeRange

private[utils] trait IntMap[+V] extends Map[Int, V]{
  override def +[V1 >: V](kv: (Int, V1)): Map[Int, V1] = toGeneralMap + kv

  override def iterator: Iterator[(Int, V)] = UnicodeRange.flatMap { e => get(e).map { v => (e, v) } }.iterator

  override def -(key: Int): Map[Int, V] = toGeneralMap - key

  private def toGeneralMap = {
    val builder = Map.newBuilder[Int, V]
    for (e <- iterator) builder += e
    builder.result()
  }
}
