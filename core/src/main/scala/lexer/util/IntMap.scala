package jp.pois.pg4scala
package lexer.util

import utils.Character.ByteRange

private[lexer] trait IntMap[+V] extends Map[Int, V] {
  override def +[V1 >: V](kv: (Int, V1)): Map[Int, V1] = toGeneralMap + kv

  override def iterator: Iterator[(Int, V)] = domain.flatMap { e => get(e).map { v => (e, v) } }

  override def -(key: Int): Map[Int, V] = toGeneralMap - key

  protected def domain: Iterator[Int] = ByteRange.iterator

  private def toGeneralMap = {
    val builder = Map.newBuilder[Int, V]
    for (e <- iterator) builder += e
    builder.result()
  }
}
