package jp.pois.pg4scala
package parser

sealed abstract class NonTerminalSymbol

object NonTerminalSymbol {
  def apply(identifier: String): NonTerminalSymbol = StringSymbol(identifier)

  private[parser] case class StringSymbol(identifier: String) extends NonTerminalSymbol
  private[parser] case class InnerSymbol(identifier: String, magic: Int) extends NonTerminalSymbol
  case object TopInitialSymbol extends NonTerminalSymbol
}

