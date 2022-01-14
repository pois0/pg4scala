package jp.pois.pg4scala
package utils

private[pg4scala] class IntCustomMap[+V](private val pred: Int => Boolean, private val value: V) extends IntMap[V] {
  override def get(key: Int): Option[V] = if (pred(key)) Some(value) else None
}
