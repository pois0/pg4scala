package jp.pois.pg4scala
package lexer.util

import java.io.StringReader

private[lexer] class StringIterableAsUtf32(str: String) extends Iterable[Int] {
  override def iterator: Iterator[Int] = new StringIteratorAsUtf32

  private class StringIteratorAsUtf32 extends Iterator[Int] {
    private val reader = new StringReader(str)
    private var tmp: Int = -1

    override def hasNext: Boolean = {
      if (tmp >= 0) return true

      val c = reader.read()
      tmp = c
      c >= 0
    }

    override def next(): Int = {
      val c = tmp
      tmp = -1
      c
    }
  }
}
