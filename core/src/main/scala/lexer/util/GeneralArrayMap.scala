package jp.pois.pg4scala
package lexer.util

private[lexer] trait GeneralArrayMap[+V] extends IntMap[V] {
  protected val domainSeq: Seq[Int]

  protected def exists(key: Int): Int = {
    var l = -1
    var h = domainSeq.size
    while (h - l > 1) {
      val mid = l + (h - l) / 2
      val v = domainSeq(mid)
      if (v < key) l = mid
      else if (v == key) return mid
      else h = mid
    }

    -1
  }
}
