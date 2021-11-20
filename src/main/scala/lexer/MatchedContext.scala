package jp.pois.pg4scala
package lexer

import lexer.MatchedContext.CharPosition

case class MatchedContext(matchedString: String, start: CharPosition, end: CharPosition)

object MatchedContext {
  implicit class MatchedContextFields(val context: MatchedContext) {
    def length: Int = context.matchedString.length
  }

  case class CharPosition(row: Int, column: Int, charIndex: Int)
}
