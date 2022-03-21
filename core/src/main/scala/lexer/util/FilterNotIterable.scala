package jp.pois.pg4scala
package lexer.util

private[lexer] class FilterNotIterable(private val origin: Iterable[Int], private val nots: Seq[Int]) extends Iterable[Int] {
  override def iterator: Iterator[Int] = new InnerIterator(origin.iterator)

  private class InnerIterator(private val origin: Iterator[Int]) extends Iterator[Int] {
    private var notsIndex: Int = 0
    private var originHasNext: Boolean = _
    private var originTmp: Int = 0

    if (origin.hasNext) {
      originHasNext = true
      originTmp = origin.next()
    } else {
      originHasNext = false
    }

    override def hasNext: Boolean = {
      if (!originHasNext) return false

      var tmp = originTmp
      var i = notsIndex
      while (i < nots.length) {
        val notElm = nots(i)
        if (tmp < notElm) {
          notsIndex = i
          originTmp = tmp
          return true
        }
        else if (tmp == notElm) {
          if (origin.hasNext) {
            tmp = origin.next()
          } else {
            originHasNext = false
            return false
          }
        } else {
          i += 1
        }
      }

      notsIndex = i
      originTmp = tmp
      true
    }

    override def next(): Int = {
      val tmp = originTmp
      if (origin.hasNext) {
        originTmp = origin.next()
      } else {
        originHasNext = false
      }
      tmp
    }
  }
}
