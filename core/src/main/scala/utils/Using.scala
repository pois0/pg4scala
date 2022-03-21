package jp.pois.pg4scala
package utils

import java.io.Closeable

private[pg4scala] object Using {
  def apply[R <: Closeable, T](closable: R)(block: R => T): T = try block(closable) finally closable.close()
}