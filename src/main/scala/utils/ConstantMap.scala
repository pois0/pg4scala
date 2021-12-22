package jp.pois.pg4scala
package utils

class ConstantMap[K, +V](val value: V) extends Map[K, V] {
  override def +[V1 >: V](kv: (K, V1)): Map[K, V1] = throw new UnsupportedOperationException

  override def get(key: K): Option[V] = Some(value)

  override def iterator: Iterator[(K, V)] = throw new UnsupportedOperationException

  override def -(key: K): Map[K, V] = throw new UnsupportedOperationException
}
