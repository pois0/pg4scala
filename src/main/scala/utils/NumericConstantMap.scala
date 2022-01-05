package jp.pois.pg4scala
package utils

private[pg4scala] class NumericConstantMap[+V](private val range: Range, private val value: V) extends Map[Int, V] {
  override def +[V1 >: V](kv: (Int, V1)): Map[Int, V1] = Map(kv) ++ this

  override def get(key: Int): Option[V] = if (range.contains(key)) Some(value) else None

  override def iterator: Iterator[(Int, V)] = new RangeMapIterator

  override def -(key: Int): Map[Int, V] = Map(this.iterator.toSeq: _*) - key

  private class RangeMapIterator extends Iterator[(Int, V)] {
    private var current = range.start

    override def hasNext: Boolean = range.contains(current)

    override def next(): (Int, V) = {
      val tmp = current
      current += 1
      (tmp, value)
    }
  }
}
