package jp.pois.pg4scala
package parser

sealed abstract class NonTerminalSymbol

object NonTerminalSymbol {
  def apply(identifier: String): NonTerminalSymbol = StringSymbol(identifier)

  private[parser] final case class StringSymbol(identifier: String) extends NonTerminalSymbol
  private[parser] final case class InnerSymbol(identifier: NonTerminalSymbol) extends NonTerminalSymbol
  case object TopInitialSymbol extends NonTerminalSymbol
}
