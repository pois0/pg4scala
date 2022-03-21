package jp.pois.pg4scala
package lexer.util

private[lexer] object SeqFuncs {
  def allSameElement[V](seq: Seq[V]): Boolean = {
    if (seq.isEmpty) return true

    val head = seq.head
    for (i <- 1 until seq.length) {
      if (head != seq(i)) return false
    }

    true
  }
}
