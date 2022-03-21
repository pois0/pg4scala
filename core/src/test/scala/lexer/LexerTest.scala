package jp.pois.pg4scala
package lexer

import common.Token
import common.Token.EOF
import lexer.LexerTest._
import lexer.MatchedContext.CharPosition
import lexer.Regex.{CharacterClass, Wildcard, not}
import lexer.Regex.CharacterClass.{Alphabet, Word}

import org.scalatest.funsuite.AnyFunSuite

import java.io.StringReader

class LexerTest extends AnyFunSuite {
  test("Simple Test") {
    val rule = Lexer.builder
      .ignore(' ', '\n', '\t')
      .rule("function", FunctionToken)
      .build

    checkString(rule)("""function function function""")(FunctionToken, FunctionToken, FunctionToken, EOF)
  }

  test("Use TokenGenerator") {
    val rule = Lexer.builder
      .ignore(' ', '\n', '\t')
      .rule("function", FunctionToken)
      .rule("if", IfToken)
      .rule(Alphabet * Word.rep0, { c => Identifier(c.matchedString) })
      .rule(CharacterClass.Digit.rep1, { c => Digit(c.matchedString.toLong) })
      .build

    checkString(rule)("""function ident 8 3 if""")(FunctionToken, Identifier("ident"), LexerTest.Digit(8), LexerTest.Digit(3L), IfToken, EOF)
  }

  test("Test CharPosition") {
    val rule = Lexer.builder
      .ignore(' ', '\n', '\t')
      .rule("function", { c => FunctionTokenWithPos(c.start, c.end) })
      .rule(Alphabet * Word.rep0, { c => IdentifierWithPos(c.matchedString, c.start, c.end) })
      .build

    checkString(rule)(
"""ident
function
ident2 function
"""
    )(
      IdentifierWithPos("ident", CharPosition(0, 0, 0), CharPosition(0, 5, 5)),
      FunctionTokenWithPos(CharPosition(1, 0, 6), CharPosition(1, 8, 14)),
      IdentifierWithPos("ident2", CharPosition(2, 0, 15), CharPosition(2, 6, 21)),
      FunctionTokenWithPos(CharPosition(2, 7, 22), CharPosition(2, 15, 30)),
      EOF
    )
  }

  test("Test CharPosition when regexp of rule includes a line break") {
    val rule = Lexer.builder
      .ignore(' ', '\n', '\t')
      .rule((Word | '\n').rep1, { c => IdentifierWithPos(c.matchedString, c.start, c.end) })
      .build

    checkString(rule)(
"""ident
ifier
"""
    )(
      IdentifierWithPos("ident\nifier\n", CharPosition(0, 0, 0), CharPosition(2, 0, 13)),
      EOF
    )
  }

  test("Test longest match") {
    val rule = Lexer.builder
      .rule("if", IfToken)
      .rule(Alphabet * Word.rep0, { c => Identifier(c.matchedString) })
      .rule(CharacterClass.Digit.rep1, { c => Digit(c.matchedString.toLong) })
      .build

    checkString(rule)("if8")(Identifier("if8"), EOF)
  }

  test("Test UTF-8") {
    val rule = Lexer.builder
      .ignore(' ', '\n', '\t')
      .rule("function", { c => FunctionTokenWithPos(c.start, c.end) })
      .rule(not(' ', '\n', '\t').rep1, { c => IdentifierWithPos(c.matchedString, c.start, c.end) })
      .build

    checkString(rule)(
      """ident
識別子 識別子🥺 test function
ident2 function
"""
    )(
      IdentifierWithPos("ident", CharPosition(0, 0, 0), CharPosition(0, 5, 5)),
      IdentifierWithPos("識別子", CharPosition(1, 0, 6), CharPosition(1, 3, 9)),
      IdentifierWithPos("識別子\uD83E\uDD7A", CharPosition(1, 4, 10), CharPosition(1, 8, 14)),
      IdentifierWithPos("test", CharPosition(1, 9, 15), CharPosition(1, 13, 19)),
      FunctionTokenWithPos(CharPosition(1, 14, 20), CharPosition(1, 22, 28)),
      IdentifierWithPos("ident2", CharPosition(2, 0, 29), CharPosition(2, 6, 35)),
      FunctionTokenWithPos(CharPosition(2, 7, 36), CharPosition(2, 15, 44)),
      EOF
    )
  }

  def checkString(lexer: Lexer)(str: String)(expected: Token*): Unit = {
    assert(lexer.lex(new StringReader(str)) == expected, s"""Testing text: "$str"""")
  }
}

object LexerTest {
  case object FunctionToken extends Token
  case object IfToken extends Token
  case class Identifier(str: String) extends Token
  case class Digit(num: Long) extends Token

  case class FunctionTokenWithPos(start: CharPosition, end: CharPosition) extends Token
  case class IdentifierWithPos(str: String, start: CharPosition, end: CharPosition) extends Token
}
