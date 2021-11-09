package jp.pois.pg4scala
package lexer

import lexer.Regex.Alternation.{EnumeratedAlternation, RangeAlternation}

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
    final case class RangeAlternation private[Regex](from: Int, to: Int) extends Alternation
    final case class EnumeratedAlternation private[Regex](es: Array[Regex]) extends Alternation
  }

  implicit def charToRegex(char: Char): Regex = Symbol(char.toInt)
  implicit def stringToRegex(string: String): Regex = Concatenation(string.map(charToRegex).toArray)
  def range(from: Char, to: Char): Regex = RangeAlternation(from, to)
  def epsilon(): Regex = Epsilon
  def wildcard(): Regex = Wildcard

  implicit class RegexFuncs(val self: Regex) {
    def |(that: Regex): Regex = EnumeratedAlternation(Array(self, that))
    def *(that: Regex): Regex = Concatenation(Array(self, that))
    def rep0(): Regex = Repetition(self)
    def rep1(): Regex = self * self.rep0()
    def opt(): Regex = epsilon | self
  }

  implicit def charToRegexFuncs(char: Char): RegexFuncs = RegexFuncs(Symbol(char.toInt))
  implicit def stringToRegexFuncs(string: String): RegexFuncs = RegexFuncs(Concatenation(string.map(charToRegex).toArray))
}
