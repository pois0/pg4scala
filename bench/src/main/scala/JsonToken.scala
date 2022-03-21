package jp.pois.pg4scala

import common.Token
import lexer.Lexer
import lexer.Regex.{CharacterClass, characterClass, not}

object JsonToken {
  sealed abstract class JsonToken extends Token
  case object LBrace extends JsonToken
  case object RBrace extends JsonToken
  case object LSquare extends JsonToken
  case object RSquare extends JsonToken
  case object Comma extends JsonToken
  case object Colon extends JsonToken
  case object True extends JsonToken
  case object False extends JsonToken
  case object Null extends JsonToken
  case object Plus extends JsonToken
  case object Minus extends JsonToken
  case object Dot extends JsonToken
  case object Exp extends JsonToken
  case class Digit(str: String) extends JsonToken
  case class Str(str: String) extends JsonToken

  def jsonLexerBuilder: Lexer.LexerBuilder = Lexer.builder
    .ignore('\u0020', '\u000A', '\u000D', '\u0009')
    .rule('{', LBrace)
    .rule('}', RBrace)
    .rule('[', LSquare)
    .rule(']', RSquare)
    .rule(',', Comma)
    .rule(':', Colon)
    .rule("true", True)
    .rule("false", False)
    .rule("null", Null)
    .rule('+', Plus)
    .rule('-', Minus)
    .rule('.', Dot)
    .rule('e', Exp)
    .rule(CharacterClass.Digit, { c => Digit(c.matchedString) })
    .rule(
      '"'
        * (
        not('\\', '"')
          | ('\\' * (
          characterClass("\"\\/bfnrt")
            | 'u' * (CharacterClass.Digit | ('a' to 'f') repExactN 4)
          ))
        ).rep0
        * '"',
      { c => Str(c.matchedString) }
    )
}
