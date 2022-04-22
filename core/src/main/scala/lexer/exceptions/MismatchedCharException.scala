package jp.pois.pg4scala
package lexer.exceptions

import lexer.MatchedContext.CharPosition

case class MismatchedCharException(mismatchedString: String, start: CharPosition, end: CharPosition) extends LexingException
