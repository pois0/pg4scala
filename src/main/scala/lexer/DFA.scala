package jp.pois.pg4scala
package lexer

import lexer.DFA.TransitionResult.{Accepted, OnGoing, Rejected}
import lexer.DFA.{State, TransitionResult}
import lexer.Lexer.TokenGenerator
import lexer.Utils.ASCII_SIZE
import utils.Math.minBy
import utils.RawArrayBuffer

import scala.collection.mutable
import scala.language.implicitConversions

case class DFA private(private val table: Array[Map[Int, State]], private val resultMap: Map[Int, TokenGenerator]) {
  def transit(currentState: State, input: Int): TransitionResult =
    table(currentState).get(input).map(OnGoing).getOrElse { currentResult(currentState) }

  def currentResult(currentState: State): TransitionResult = resultMap.get(currentState).map(Accepted).getOrElse(Rejected)
}

object DFA {
  private[lexer] def fromNFA(nfa: NFA): DFA = {
    val initialState = Set(nfa.initialState)
    val queue = mutable.Queue((initialState, 0))

    val transitTable = new RawArrayBuffer[Map[Int, State]](nfa.table.size)
    val stateMap = mutable.Map(initialState -> 0)
    var newStateId = 1
    val resultMap = mutable.Map.empty[Int, TokenGenerator]
    while (queue.nonEmpty) {
      val (nfaState, dfaState) = queue.dequeue()
      nfaState.flatMap { s => nfa.resultMap.get(s) }.reduceOption(minBy(nfa.resultOrder)).foreach {
        resultMap(dfaState) = _
      }

      transitTable(dfaState) = {
        val map = mutable.Map.empty[Int, Set[Int]]

        nfaState.foreach { state =>
          for ((c, seq) <- nfa.table(state)) {
            map(c) = map.getOrElse(c, Set.empty) ++ seq
          }
        }

        map.map { case (c, stateSet) =>
          val index = stateMap.getOrElseUpdate(stateSet, {
            val tmp = newStateId
            newStateId += 1
            queue += Tuple2(stateSet, tmp)
            tmp
          })
          c -> State(index)
        }.toMap
      }
    }

    new DFA(transitTable.toArray, resultMap.toMap)
  }

  sealed abstract class TransitionResult

  object TransitionResult {
    case class OnGoing(state: State) extends TransitionResult
    case class Accepted(tokenGen: TokenGenerator) extends TransitionResult
    object Rejected extends TransitionResult
  }

  implicit class State(val value: Int) extends AnyVal

  object State {
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
