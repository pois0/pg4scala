package jp.pois.pg4scala
package lexer.util

import utils.Character.ByteRange

private[lexer] trait IntMap[+V] extends Map[Int, V] {
  override def updated[V1 >: V](key: Int, value: V1): Map[Int, V1] = toGeneralMap.updated(key, value)

  override def iterator: Iterator[(Int, V)] = domain.flatMap { e => get(e).map { v => (e, v) } }

  override def removed(key: Int): Map[Int, V] = toGeneralMap - key

  protected def domain: Iterator[Int] = ByteRange.iterator

  private def toGeneralMap = {
    val builder = Map.newBuilder[Int, V]
    for (e <- iterator) builder += e
    builder.result()
  }
}
