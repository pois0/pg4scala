package jp.pois.pg4scala
package parser

import common.Token

sealed abstract class Term

object Term {
  import scala.language.implicitConversions
  import scala.reflect.ClassTag

  case class Terminal(clazz: Class[_]) extends Term {
    def check(token: Token): Boolean = clazz.isInstance(token)
  }

  case class NonTerminal(symbol: NonTerminalSymbol) extends Term

  case class NonTerminalSymbol private(identifier: String) extends AnyVal

  object Terminal {
    def apply[A <: Token](implicit tag: ClassTag[A]): Terminal = new Terminal(tag.runtimeClass)
    val EOF: Terminal = Terminal[Token.EOF.type]
  }

  implicit def tokenToTerm(token: Token): Term = new Terminal(token.getClass)
  implicit def nonTerminalSymbolToTerm(symbol: NonTerminalSymbol): Term = NonTerminal(symbol)

  object NonTerminalSymbol {
    def apply(identifier: String): NonTerminalSymbol = {
      if (identifier.isEmpty) throw new IllegalArgumentException("identifier must be non-empty.")
      new NonTerminalSymbol(identifier)
    }

    val SpecialNonTerminal = new NonTerminalSymbol("")
  }
}
