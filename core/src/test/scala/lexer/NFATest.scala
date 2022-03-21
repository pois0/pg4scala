package jp.pois.pg4scala
package lexer

import common.Token
import lexer.NFATest.{TestToken, transit}
import lexer.Regex.{CharacterClass, characterClass, not, notInt, stringToRegexFuncs}

import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets

class NFATest extends AnyFunSuite {
  test("Symbol") {
    nfaCheck('a')("a")("b", "", "af")
    nfaCheck('あ')("あ")("a", " ")
  }

  test("Concatenate") {
    nfaCheck("hello")("hello")("", "bye", "hello!!")
  }

  test("CharacterClassAlternation") {
    nfaCheck(characterClass("and"))("a", "n", "d")("", "k", "an", "ad", "and")
  }

  test("RangeAlternation") {
    nfaCheck('a' to 'd')("a", "b", "c", "d")("e", "f", "", "abc")
  }

  test("Not") {
    nfaCheck(not('a'))("b", "f", "\u0080", "\u0800", "\uD800\uDC00")("a", "", "bf")
  }

  //noinspection SpellCheckingInspection
  test("EnumeratedAlternation") {
    nfaCheck("red" | "black" | "white")("red", "black", "white")("yellow", "re", "blackblack")
    nfaCheck("red".opt)("", "red")("yellow", "re", "redred")
  }

  //noinspection SpellCheckingInspection
  test("Repetition") {
    nfaCheck("hello".rep0)("", "hello", "hellohello", "hellohellohellohello")("he", "hellohe", "ho")
    nfaCheck("hello".rep1)("hello", "hellohello", "hellohellohellohello")("", "he", "hellohe", "ho")
    nfaCheck("hello" repN 5)("hellohellohellohellohello", "hellohellohellohellohellohellohello")("", "hello", "hellohe")
  }

  test("Epsilon") {
    nfaCheck(Regex.epsilon)("")("a", "100")
  }

  test("wildcard") {
    nfaCheck(Regex.wildcard)("a", "1", " ", "\u0080", "\u0800", "\uD800\uDC00")("", "hello")
  }

  test("predefined regexes") {
    nfaCheck(CharacterClass.WhiteSpace)(" ", "\t", "\n", "\u000B", "\f", "\r")("", "a", "  ", "あ")
    nfaCheck(CharacterClass.Digit)((0 to 9).map { _.toString }:_*)("", "a", "/", ":", "あ")
    nfaCheck(CharacterClass.UpperAlphabet)(('A' to 'Z').map { _.toString }:_*)()
    nfaCheck(CharacterClass.LowerAlphabet)(('a' to 'z').map { _.toString }:_*)()
    nfaCheck(CharacterClass.Alphabet)()()
    nfaCheck(CharacterClass.Word)()()
  }

  private def nfaCheck(regex: Regex)(shouldBeAccepted: String*)(shouldBeRejected: String*): Unit = {
    val nfa = NFA.fromRegexes((regex, { _ => TestToken }))
    for (str <- shouldBeAccepted) {
      val transitRes = transit(nfa, str)
      assert(
        transitRes.exists(nfa.resultMap.contains),
        s"""Checking whether "$str" is accepted by $regex / transitRes: $transitRes / nfa: $nfa"""
      )
    }

    for (str <- shouldBeRejected) {
      val transitRes = transit(nfa, str)
      assert(
        !transitRes.exists(nfa.resultMap.contains),
        s"""Checking whether "$str" is accepted by $regex / transitRes: $transitRes / nfa: $nfa"""
      )
    }
  }
}

object NFATest {
  private object TestToken extends Token

  private def transit(nfa: NFA, string: String): Set[Int] =
    string.getBytes(StandardCharsets.UTF_8).foldLeft(Set(nfa.initialState)) { (states, c) =>
      states.flatMap { s => nfa.table(s).getOrElse(c & 0xff, Set.empty) }
    }
}
