package jp.pois.pg4scala
package utils

import scala.collection.mutable.ArrayBuffer

class RawArrayBuffer[A](initialSize: Int) extends ArrayBuffer[A](initialSize) {
  size0 = initialSize

  override def update(idx: Int, elem: A): Unit = {
    ensureSize(idx)
    array(idx) = elem.asInstanceOf[AnyRef]
  }
}
