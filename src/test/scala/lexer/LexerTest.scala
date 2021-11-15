package jp.pois.pg4scala
package lexer

import common.Token
import lexer.LexerTest._
import lexer.Regex.CharacterClass
import lexer.Regex.CharacterClass.{Alphabet, Word}

import org.scalatest.funsuite.AnyFunSuite

import java.io.StringReader

class LexerTest extends AnyFunSuite {
  test("rule1") {
    checkString(rule1)("""function function function""")(FunctionToken, FunctionToken, FunctionToken)
  }

  test("rule2") {
    checkString(rule2)("""function ident 8 3 if""")(FunctionToken, Identifier("ident"), LexerTest.Digit(8), LexerTest.Digit(3L), IfToken)
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

  val rule1 = Lexer.builder()
    .ignore(' ', '\n', '\t')
    .rule("function", FunctionToken)
    .build

  val rule2 = Lexer.builder()
    .ignore(' ', '\n', '\t')
    .rule("function", FunctionToken)
    .rule("if", IfToken)
    .rule(Alphabet * Word.rep0, Identifier)
    .rule(CharacterClass.Digit.rep1, { str => Digit(str.toLong) })
    .build
}
