package jp.pois.pg4scala
package utils

object Character {
  @inline
  def ByteRange: Range = 0 to 0xff

  @inline
  def UnicodeRange: Range = 0 to 0x10ffff
}
