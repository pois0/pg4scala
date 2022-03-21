package jp.pois.pg4scala
package lexer.util

private[lexer] class ArrayPairMap[+V](override val domainSeq: Seq[Int], private val range: Seq[V]) extends IntMap[V] with GeneralArrayMap[V] {
  override def iterator: Iterator[(Int, V)] = domainSeq.iterator.zip(range.iterator)

  override def get(key: Int): Option[V] = {
    val e = exists(key)
    if (e >= 0) Some(range(e)) else None
  }
}
