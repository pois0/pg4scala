package jp.pois.pg4scala
package parser

import common.Token

sealed abstract class Term

object Term {
  import scala.language.implicitConversions
  import scala.reflect.ClassTag

  type TokenType = Class[_]

  case class Terminal(clazz: TokenType) extends Term {
    def check(token: Token): Boolean = clazz.isInstance(token)

    override def equals(obj: Any): Boolean = obj match {
      case other: Terminal => this.clazz == other.clazz
      case _ => false
    }
  }

  case class NonTerminal(symbol: NonTerminalSymbol) extends Term

  object Terminal {
    def apply[A <: Token](implicit tag: ClassTag[A]): Terminal = new Terminal(tag.runtimeClass)
    private[parser] val EOF: Terminal = Terminal[Token.EOF.type]
    private[parser] val EOFType = Token.EOF.getClass
    private[parser] val Sharp = Terminal[SharpToken.type]
    private[parser] val SharpClass = SharpToken.getClass

    private object SharpToken extends Token
  }

  implicit def tokenToTerm(token: Token): Term = new Terminal(token.getClass)
  implicit def nonTerminalSymbolToTerm(symbol: NonTerminalSymbol): Term = NonTerminal(symbol)
}
