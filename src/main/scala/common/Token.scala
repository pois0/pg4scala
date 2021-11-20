package jp.pois.pg4scala
package common

trait Token

object Token {
  object Skip extends Token
  object EOF extends Token
}
