package jp.pois.pg4scala
package lexer.util

private[lexer] class ArrayMap[+V](override val domainSeq: Seq[Int], private val value: V) extends IntMap[V] with GeneralArrayMap[V] {
  override def iterator: Iterator[(Int, V)] = domainSeq.iterator.map { (_, value) }

  override def get(key: Int): Option[V] = if (exists(key) >= 0) Some(value) else None
}
