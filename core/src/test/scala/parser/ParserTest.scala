package jp.pois.pg4scala
package parser

import parser.ParserTest.Tokens._
import parser.ParserTest.Value.{Equals, Unary}
import parser.ParserTest._

import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {
  // The rule is quoted from "Modern Compiler Implementation in ML (New Edition)" (A. Appel)
  test("LR(0) Test") {
    val rule = Parser.builder[Value](S)
      .rule(S, Array(LParen, L, RParen), { seq => seq(1).asValue })
      .rule(S, Array(Tokens.Id), { _ => Value.Id })
      .rule(L, Array(S), { seq => Value.Cons(Value.Empty, seq(0).asValue) })
      .rule(L, Array(L, Comma, S), { seq => Value.Cons(seq(0).asValue.asInstanceOf[Value.List], seq(2).asValue) })
      .build

    testParse(rule, Tokens.Id #:: common.Token.EOF #:: LazyList.empty, Value.Id)
    testParse(rule, LParen #:: Tokens.Id #:: RParen #:: common.Token.EOF #:: LazyList.empty, Value.Cons(Value.Empty, Value.Id))
    testParse(rule, LParen #:: LParen #:: Tokens.Id #:: Comma #:: Tokens.Id #:: RParen #:: RParen #:: common.Token.EOF #:: LazyList.empty, Value.Cons(Value.Empty, Value.Cons(Value.Cons(Value.Empty, Value.Id), Value.Id)))
  }

  // The rule is quoted from "Compilers: Principles, Techniques, and Tools (Second Edition)" (A. Aho, et al.)
  test("LALR(1) Test") {
    val rule = Parser.builder[Value](S)
      .rule(S, Array(L, Equal, R), { seq => Value.Equals(seq(0).asValue, seq(2).asValue) })
      .rule(S, Array(R), { seq => seq(0).asValue })
      .rule(L, Array(Star, R), { seq => Unary(seq(1).asValue) })
      .rule(L, Array(Tokens.Id), { seq => Value.Id })
      .rule(R, Array(L), { seq => seq(0).asValue })
      .build

    testParse(rule, Tokens.Id #:: common.Token.EOF #:: LazyList.empty, Value.Id)
    testParse(rule, Tokens.Star #:: Tokens.Id #:: common.Token.EOF #:: LazyList.empty, Unary(Value.Id))
    testParse(rule, Tokens.Id #:: Equal #:: Tokens.Id #:: common.Token.EOF #:: LazyList.empty, Equals(Value.Id, Value.Id))
    testParse(rule, Tokens.Star #:: Tokens.Star #:: Tokens.Star #:: Tokens.Id #:: common.Token.EOF #:: LazyList.empty, Unary(Unary(Unary(Value.Id))))
  }

  private def testParse[Value](parser: Parser[Value], tokens: LazyList[common.Token], expected: Value) = {
    val actual = parser.parse(tokens)
    assert(expected == actual)
  }
}

//noinspection TypeAnnotation
object ParserTest {
  val S = NonTerminalSymbol("S")
  val L = NonTerminalSymbol("L")
  val R = NonTerminalSymbol("R")

  object Tokens {
    case object RParen extends common.Token
    case object LParen extends common.Token
    case object Id extends common.Token
    case object Comma extends common.Token
    case object Equal extends common.Token
    case object Star extends common.Token
  }

  sealed abstract class Value

  object Value {
    case object Id extends Value

    sealed abstract class List extends Value
    case class Cons(rest: List, head: Value) extends List
    case object Empty extends List

    case class Equals(left: Value, right: Value) extends Value
    case class Unary(rand: Value) extends Value
  }
}
