package jp.pois.pg4scala
package lexer.util

import java.util

class RawIntArrayBuffer(initialSize: Int) {
  private var arr = new Array[Int](initialSize)

  def update(idx: Int, value: Int): Unit = {
    ensureSize(idx + 1)
    arr(idx) = value
  }

  def toArray(size: Int): Array[Int] = {
    util.Arrays.copyOf(arr, size)
  }

  private def ensureSize(size: Int): Unit = {
    if (size > arr.length) {
      val newSize = size * 2
      arr = util.Arrays.copyOf(arr, newSize)
    }
  }
}
