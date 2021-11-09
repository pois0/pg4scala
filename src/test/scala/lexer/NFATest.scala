package jp.pois.pg4scala
package lexer

import lexer.NFA.{epsilon, fromRegex}
import lexer.NFATest.transit
import lexer.Regex.{range, stringToRegexFuncs}

import org.scalatest.funsuite.AnyFunSuite

class NFATest extends AnyFunSuite {
  test("Symbol") {
    nfaCheck('a')("a")("b", "", "af")
  }

  test("Concatenate") {
    nfaCheck("hello")("hello")("", "bye", "hello!!")
  }

  test("EnumeratedAlternation") {
    nfaCheck("red" | "black" | "white")("red", "black", "white")("yellow", "re", "black_black")
  }

  test("RangeAlternation") {
    nfaCheck(range('a', 'd'))("a", "b", "c", "d")("e", "f", "", "abc")
  }

  test("Repetition") {
    nfaCheck("hello".rep0())("", "hello", "hellohello", "hellohellohellohello")("he", "hellohe", "ho")
  }

  test("Epsilon") {
    nfaCheck(Regex.epsilon())("")("a", "100")
  }

  private def nfaCheck(regex: Regex)(shouldBeAccepted: String*)(shouldBeRejected: String*): Unit = {
    val nfa = fromRegex(regex)
    for (str <- shouldBeAccepted) {
      assert(transit(nfa, str).contains(1), s"""Checking whether "$str" will be accepted by $regex""")
    }

    for (str <- shouldBeRejected) {
      assert(!transit(nfa, str).contains(1), s"""Checking whether "$str" will be rejected by $regex""")
    }
  }
}

object NFATest {
  private def transit(nfa: NFA, string: String): Set[Int] =
    string.foldLeft(epsilonTransit(nfa)(0)) { (states, c) =>
      states.flatMap(epsilonTransit(nfa)).flatMap { s => nfa.table(s).getOrElse(c.toInt, Set.empty) }
    }.flatMap(epsilonTransit(nfa))

  private def epsilonTransit(nfa: NFA)(state: Int): Set[Int] =
    Set(state) ++ nfa.table(state).get(epsilon).fold(Set.empty[Int]) {
      _.foldLeft(Set.empty[Int]) { (states, s) => states ++ epsilonTransit(nfa)(s) }
    }
}
