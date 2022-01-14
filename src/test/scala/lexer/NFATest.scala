package jp.pois.pg4scala
package lexer

import common.Token
import lexer.NFATest.{TestToken, transit}
import lexer.Regex.{characterClass, customCharacterClass, stringToRegexFuncs}

import org.scalatest.funsuite.AnyFunSuite

class NFATest extends AnyFunSuite {
  test("Symbol") {
    nfaCheck('a')("a")("b", "", "af")
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

  test("CustomSingleAlternation") {
    nfaCheck(customCharacterClass({ _ % 2 == 0 }))(" ", "4", "N")("  ", "", "!")
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
  }

  test("Epsilon") {
    nfaCheck(Regex.epsilon)("")("a", "100")
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
    string.foldLeft(Set(nfa.initialState)) { (states, c) =>
      states.flatMap { s => nfa.table(s).getOrElse(c.toInt, Set.empty) }
    }
}
