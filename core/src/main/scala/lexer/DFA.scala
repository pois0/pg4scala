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

private[lexer] final case class DFA private(private val table: Array[Map[Int, State]], private val resultMap: Map[Int, TokenGenerator]) {
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
      nfaState.flatMap { s => nfa.resultMap.get(s) }
        .reduceOption(minBy(nfa.resultOrder))
        .foreach { resultMap(dfaState) = _ }

      transitTable(dfaState) = {
        val map = mutable.Map.empty[Int, Set[Int]]

        for (state <- nfaState; (c, seq) <- nfa.table(state)) {
          map(c) = map.getOrElse(c, Set.empty) ++ seq
        }

        map.map { case (c, stateSet) =>
          c -> State(
            stateMap.getOrElseUpdate(stateSet, {
              val tmp = newStateId
              newStateId += 1
              queue += Tuple2(stateSet, tmp)
              tmp
            })
          )
        }.toMap
      }
    }

    val dfa = new DFA(transitTable.toArray, resultMap.toMap)
    dfa
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

  private class TransitionTable(private val table: Array[Int]) {
    def apply(currentState: State, input: Int): State = table(currentState * ASCII_SIZE + input)
  }
}
