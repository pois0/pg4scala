package jp.pois.pg4scala
package lexer

import lexer.Regex.Alternation.{CustomFilterAlternation, EnumeratedAlternation}
import lexer.util.{FilterNotIterable, StringIterableAsUtf32}
import utils.Character.UnicodeRange

import java.nio.charset.StandardCharsets
import scala.collection.immutable.NumericRange

sealed abstract class Regex

object Regex {
  import scala.language.implicitConversions

  final case class Symbol8 private(c: Byte) extends Regex
  final case class Symbol32 private(c: Int) extends Regex
  sealed abstract class Alternation extends Regex
  final case class Concatenation private(es: Seq[Regex]) extends Regex
  final case class Repetition private(e: Regex) extends Regex
  final object Epsilon extends Regex
  final object Wildcard extends Regex

  object Alternation {
    final case class EnumeratedAlternation private[Regex](es: Seq[Regex]) extends Alternation
    final case class CustomFilterAlternation private[Regex](iter: Iterable[Int]) extends Alternation
  }

  object CharacterClass {
    def WhiteSpace: Regex = characterClass(0x20, 0x9, 0xa, 0xb, 0xc, 0xd)
    def Digit: Regex = 0x30 to 0x39
    def UpperAlphabet: Regex = 0x41 to 0x5a
    def LowerAlphabet: Regex = 0x61 to 0x7A
    def Alphabet: Regex = customCharacterFilterInt((0x41 to 0x5a) ++ (0x61 to 0x7A))
    def Word: Regex = customCharacterFilterInt((0x30 to 0x39) ++ (0x41 to 0x5a) ++ (0x61 to 0x7a))
  }

  implicit def charToRegex(char: Char): Regex = Symbol32(char.toInt)
  implicit def stringToRegex(string: String): Regex = Concatenation(string.getBytes(StandardCharsets.UTF_8).map(Symbol8))
  implicit def charRangeToRegex(range: NumericRange[Char]): Regex = intRangeToRegex(toRange(range))
  implicit def intRangeToRegex(range: Range): Regex = CustomFilterAlternation(range)
  def characterClass(chars: Char*): Regex = CustomFilterAlternation(chars.map { _.toInt }.sorted)
  def characterClassInt(chars: Int*): Regex = CustomFilterAlternation(chars.sorted)
  def characterClass(chars: String): Regex = CustomFilterAlternation(new StringIterableAsUtf32(chars).toArray.sorted)
  def customCharacterFilter(iter: Iterator[Char]): Regex = customCharacterFilterInt(iter.map { _.toInt })
  def customCharacterFilterInt(iter: Iterator[Int]): Regex = CustomFilterAlternation(iter.toSeq.sorted)
  def customCharacterFilter(iter: Iterable[Char]): Regex = customCharacterFilter(iter.iterator)
  def customCharacterFilterInt(iter: Iterable[Int]): Regex = customCharacterFilterInt(iter.iterator)
  def not(chars: Char*): Regex = CustomFilterAlternation(new FilterNotIterable(UnicodeRange, chars.map { _.toInt }.sorted))
  def notInt(chars: Int*): Regex = CustomFilterAlternation(new FilterNotIterable(UnicodeRange, chars.sorted))
  def epsilon: Regex = Epsilon
  def wildcard: Regex = Wildcard

  implicit final class RegexFuncs(val self: Regex) extends AnyVal {
    def |(that: Regex): Regex = EnumeratedAlternation(Seq(self, that))
    def *(that: Regex): Regex = Concatenation(Seq(self, that))
    def rep0: Regex = Repetition(self)
    def rep1: Regex = self * self.rep0
    def repN(n: Int): Regex = Concatenation(Seq.fill(n)(self) :+ self.rep0)
    def repExactN(n: Int): Regex = Concatenation(Seq.fill(n)(self))
    def opt: Regex = epsilon | self
  }

  implicit def charToRegexFuncs(char: Char): RegexFuncs = RegexFuncs(Symbol32(char.toInt))
  implicit def stringToRegexFuncs(string: String): RegexFuncs = RegexFuncs(stringToRegex(string))
  implicit def charRangeToRegexFuncs(range: NumericRange[Char]): RegexFuncs = RegexFuncs(charRangeToRegex(range))
  implicit def intRangeToRegexFuncs(range: Range): RegexFuncs = RegexFuncs(intRangeToRegex(range))

  private def toRange(range: NumericRange[Char]): Range = {
    if (range.isInclusive) Range.inclusive(range.start, range.end, range.step) else Range(range.start, range.end, range.step)
  }
}
