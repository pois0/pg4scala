package jp.pois.pg4scala
package lexer

import common.Token
import lexer.DFA.State.errorState
import lexer.DFA.TransitionResult.{Accepted, OnGoing, Rejected}
import lexer.DFA.{State, TransitionResult, TransitionTable}
import lexer.Utils.ASCII_SIZE

import scala.language.implicitConversions

class DFA private(private val table: TransitionTable, private val resultMap: Array[Option[String => Token]]) {
  def transit(currentState: State, input: Int): TransitionResult = {
    val nextState = try {
      table(currentState, input)
    } catch {
      case e: ArrayIndexOutOfBoundsException => throw new IllegalStateException("Failed to build a DFA", e)
    }

    table.getClass.getCanonicalName

    if (nextState == errorState) currentResult(currentState): @inline else OnGoing(nextState)
  }

  def currentResult(currentState: State): TransitionResult = resultMap(currentState).map(Accepted).getOrElse(Rejected)
}

object DFA {
  def fromRegex(regex: Regex): DFA = ???

  sealed abstract class TransitionResult

  object TransitionResult {
    case class OnGoing(state: State) extends TransitionResult
    case class Accepted(tokenGen: String => Token) extends TransitionResult
    object Rejected extends TransitionResult
  }

  implicit class State(val value: Int) extends AnyVal

  object State {
    private[DFA] val errorState: State = State(-1)
    val initialState: State = State(0)
  }

  private implicit def stateToInt(state: State): Int = state.value

  implicit class DFAExtension(private val dfa: DFA) {
    def transit(currentState: State, input: Char): TransitionResult = {
      val inputInt = input.toInt
      if (input >= ASCII_SIZE) throw new IllegalArgumentException // TODO Exception message
      dfa.transit(currentState, inputInt)
    }
  }

  private class TransitionTable(private val table: Array[Int]) {
    def apply(currentState: State, input: Int): State = table(currentState * ASCII_SIZE + input)
  }
}
