package jp.pois.pg4scala
package lexer

import lexer.Regex.Alternation.{EnumeratedAlternation, RangeAlternation}

import scala.collection.immutable.NumericRange

sealed abstract class Regex

object Regex {
  import scala.language.implicitConversions

  final case class Symbol private(c: Int) extends Regex
  sealed abstract class Alternation extends Regex
  final case class Concatenation private(es: Array[Regex]) extends Regex
  final case class Repetition private(e: Regex) extends Regex
  final object Epsilon extends Regex
  final object Wildcard extends Regex

  object Alternation {
    final case class RangeAlternation private[Regex](range: NumericRange[Char]) extends Alternation
    final case class EnumeratedAlternation private[Regex](es: Array[Regex]) extends Alternation
  }

  object CharacterClass {
    val Digit: Regex = '0' to '9'
    val UpperAlphabet: Regex = 'A' to 'Z'
    val LowerAlphabet: Regex = 'a' to 'z'
    val Alphabet: Regex = UpperAlphabet | LowerAlphabet
    val Word: Regex = EnumeratedAlternation(Array(Digit, UpperAlphabet, LowerAlphabet))
  }

  implicit def charToRegex(char: Char): Regex = Symbol(char.toInt)
  implicit def stringToRegex(string: String): Regex = Concatenation(string.map(charToRegex).toArray)
  implicit def charRangeToRegex(range: NumericRange[Char]): Regex = RangeAlternation(range)
  def epsilon(): Regex = Epsilon
  def wildcard(): Regex = Wildcard

  implicit class RegexFuncs(val self: Regex) {
    def |(that: Regex): Regex = EnumeratedAlternation(Array(self, that))
    def *(that: Regex): Regex = Concatenation(Array(self, that))
    def rep0: Regex = Repetition(self)
    def rep1: Regex = self * self.rep0
    def opt: Regex = epsilon | self
  }

  implicit def charToRegexFuncs(char: Char): RegexFuncs = RegexFuncs(Symbol(char.toInt))
  implicit def stringToRegexFuncs(string: String): RegexFuncs = RegexFuncs(Concatenation(string.map(charToRegex).toArray))
}
