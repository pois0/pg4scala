package jp.pois.pg4scala
package lexer

import lexer.NFA.{epsilon, finishState, initialState}
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
    nfaCheck("red" | "black" | "white")("red", "black", "white")("yellow", "re", "blackblack")
    nfaCheck("red".opt())("", "red")("yellow", "re", "redred")
  }

  test("RangeAlternation") {
    nfaCheck(range('a', 'd'))("a", "b", "c", "d")("e", "f", "", "abc")
  }

  test("Repetition") {
    nfaCheck("hello".rep0())("", "hello", "hellohello", "hellohellohellohello")("he", "hellohe", "ho")
    nfaCheck("hello".rep1())("hello", "hellohello", "hellohellohellohello")("", "he", "hellohe", "ho")
  }

  test("Epsilon") {
    nfaCheck(Regex.epsilon())("")("a", "100")
  }

  private def nfaCheck(regex: Regex)(shouldBeAccepted: String*)(shouldBeRejected: String*): Unit = {
    val nfa = NFA.fromRegex(regex)
    for (str <- shouldBeAccepted) {
      assert(transit(nfa, str).contains(finishState), s"""Checking whether "$str" is accepted by $regex""")
    }

    for (str <- shouldBeRejected) {
      assert(!transit(nfa, str).contains(finishState), s"""Checking whether "$str" is rejected by $regex""")
    }
  }
}

object NFATest {
  private def transit(nfa: NFA, string: String): Set[Int] =
    string.foldLeft(epsilonTransit(nfa)(initialState)) { (states, c) =>
      states.flatMap(epsilonTransit(nfa)).flatMap { s => nfa.table(s).getOrElse(c.toInt, Set.empty) }
    }.flatMap(epsilonTransit(nfa))

  private def epsilonTransit(nfa: NFA)(state: Int): Set[Int] =
    Set(state) ++ nfa.table(state).get(epsilon).fold(Set.empty[Int]) {
      _.foldLeft(Set.empty[Int]) { (states, s) => states ++ epsilonTransit(nfa)(s) }
    }
}
