package jp.pois.pg4scala
package parser

import common.Token

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

sealed abstract class Character

object Character {
  import scala.language.implicitConversions
  import scala.reflect.ClassTag

  type TokenType = Class[_]

  final case class Terminal(clazz: TokenType) extends Character {
    def check(token: Token): Boolean = clazz.isInstance(token)

    override def equals(obj: Any): Boolean = obj match {
      case other: Terminal => this.clazz == other.clazz
      case _ => false
    }
  }

  final case class NonTerminal(symbol: NonTerminalSymbol) extends Character

  object Terminal {
    def apply[A <: Token](implicit tag: ClassTag[A]): Terminal = new Terminal(tag.runtimeClass)
    private[parser] val EOF: Terminal = Terminal[Token.EOF.type]
    private[parser] val EOFType = Token.EOF.getClass
    private[parser] val Sharp = Terminal[SharpToken.type]
    private[parser] val SharpClass = SharpToken.getClass

    private object SharpToken extends Token
  }

  implicit def tokenToTerm(token: Token): Character = new Terminal(token.getClass)
  implicit def nonTerminalSymbolToTerm(symbol: NonTerminalSymbol): Character = NonTerminal(symbol)

  def charSeq(chars: Character*): Seq[Character] = chars
  def tokenSeq(chars: Token*): Seq[Character] = chars.map(tokenToTerm)
  def ntSeq(chars: NonTerminalSymbol*): Seq[Character] = chars.map(nonTerminalSymbolToTerm)
}
