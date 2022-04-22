package jp.pois.pg4scala
package lexer.exceptions

import lexer.MatchedContext.CharPosition

case class MismatchedCharException(
  bufferedString: String,
  currentChar: Int,
  start: CharPosition,
  end: CharPosition
)extends LexingException
