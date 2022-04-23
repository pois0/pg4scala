package jp.pois.pg4scala
package lexer.util

import scala.collection.mutable.ArrayBuffer

private[pg4scala] class RawArrayBuffer[A](initialSize: Int) extends ArrayBuffer[A](initialSize) {
  size0 = initialSize

  override def update(idx: Int, elem: A): Unit = {
    ensureSize(idx + 1)
    size0 = idx + 1
    array(idx) = elem.asInstanceOf[AnyRef]
  }

  def expand(size: Int): Unit = {
    ensureSize(size)
    size0 = size
  }
}
