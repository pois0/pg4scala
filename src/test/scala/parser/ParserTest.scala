package jp.pois.pg4scala
package parser

import parser.ParserTest.Tokens.{Comma, LParen, RParen}
import parser.ParserTest.{L, S, Tokens, Value}
import parser.Term.NonTerminalSymbol

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {
  test("Simple Test") {
    val rule = Parser.builder[Value]
      .initialTerm(S)
      .rule(S, Array(LParen, L, RParen), { seq => seq(1).asValue })
      .rule(S, Array(Tokens.Id), { _ => Value.Id })
      .rule(L, Array(S), { seq => Value.Cons(Value.Empty, seq(0).asValue) })
      .rule(L, Array(L, Comma, S), { seq => Value.Cons(seq(0).asValue.asInstanceOf[Value.List], seq(2).asValue) })
      .build

    testParse(rule, Tokens.Id #:: common.Token.EOF #:: Stream.Empty, Value.Id)
    testParse(rule, LParen #:: Tokens.Id #:: RParen #:: common.Token.EOF #:: Stream.Empty, Value.Cons(Value.Empty, Value.Id))
    testParse(rule, LParen #:: LParen #:: Tokens.Id #:: Comma #:: Tokens.Id #:: RParen #:: RParen #:: common.Token.EOF #:: Stream.Empty, Value.Cons(Value.Empty, Value.Cons(Value.Cons(Value.Empty, Value.Id), Value.Id)))
  }

  private def testParse[Value](parser: Parser[Value], tokens: Stream[common.Token], expected: Value) = {
    val actual = parser.parse(tokens)
    assert(expected == actual)
  }
}

object ParserTest {
  val S = NonTerminalSymbol("S")
  val L = NonTerminalSymbol("L")

  object Tokens {
    case object RParen extends common.Token
    case object LParen extends common.Token
    case object Id extends common.Token
    case object Comma extends common.Token
  }

  sealed abstract class Value

  object Value {
    case object Id extends Value

    sealed abstract class List extends Value
    case class Cons(rest: List, head: Value) extends List
    case object Empty extends List
  }
}
