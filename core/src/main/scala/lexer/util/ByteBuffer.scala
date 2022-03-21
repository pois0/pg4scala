package jp.pois.pg4scala
package lexer.util

import lexer.util.ByteBuffer.{MAX_ARRAY_SIZE, hugeCapacity}

import java.util

class ByteBuffer(initialSize: Int = 4000) {
  private var buf = new Array[Byte](initialSize)
  private var index = 0

  private def ensureCapacity(minCapacity: Int): Unit = {
    if (minCapacity - buf.length > 0) grow(minCapacity)
  }

  private def grow(minCapacity: Int): Unit = {
    val oldCapacity = buf.length
    var newCapacity = oldCapacity << 1
    if (newCapacity - minCapacity < 0) newCapacity = minCapacity
    if (newCapacity - MAX_ARRAY_SIZE > 0) newCapacity = hugeCapacity(minCapacity)
    buf = util.Arrays.copyOf(buf, newCapacity)
  }
}

private object ByteBuffer {
  private val MAX_ARRAY_SIZE = Int.MaxValue - 8

  private def hugeCapacity(minCapacity: Int): Int = {
    if (minCapacity < 0) throw new OutOfMemoryError()
    if (minCapacity > MAX_ARRAY_SIZE) Int.MaxValue else MAX_ARRAY_SIZE
  }
}
