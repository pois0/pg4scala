package jp.pois.pg4scala
package lexer

import lexer.DFA.TransitionResult.{Accepted, OnGoing, Rejected}
import lexer.DFA.{State, TransitionResult}
import lexer.Lexer.TokenGenerator
import lexer.util.RawIntArrayBuffer
import utils.Math.minBy

import scala.collection.immutable.{TreeMap, TreeSet}
import scala.collection.mutable
import scala.language.implicitConversions

private[lexer] final case class DFA private(private val table: Array[Int], private val resultMap: Map[Int, TokenGenerator]) {
  def transit(currentState: State, input: Int): TransitionResult = {
    val next = table(currentState * 0x100 + input)
    if (next == State.jamState) {
      currentResult(currentState)
    } else {
      OnGoing(next)
    }
  }

  def currentResult(currentState: State): TransitionResult = resultMap.get(currentState).map(Accepted).getOrElse(Rejected)
}

object DFA {
  private[lexer] def fromNFA(nfa: NFA): DFA = {
    val initialState = TreeSet(nfa.initialState)
    val queue = mutable.Queue((initialState, 1))

    val transitTable = new RawIntArrayBuffer(nfa.table.size * 0x100)
    val stateMap = mutable.Map(initialState -> 1)
    var newStateId = 2
    val resultMap = mutable.TreeMap.empty[Int, TokenGenerator]
    while (queue.nonEmpty) {
      val (nfaState, dfaState) = queue.dequeue()
      nfaState.iterator.flatMap { s => nfa.resultMap.get(s) }
        .reduceOption(minBy(nfa.resultOrder))
        .foreach { resultMap(dfaState) = _ }

      val map = mutable.Map.empty[Int, TreeSet[Int]]

      for (state <- nfaState; (c, seq) <- nfa.table(state)) {
        map(c) = map.getOrElse(c, TreeSet.empty[Int]) ++ seq
      }

      for ((c, stateSet) <- map) {
        transitTable(dfaState * 0x100 + c) = State(
          stateMap.getOrElseUpdate(stateSet, {
            val tmp = newStateId
            newStateId += 1
            queue += Tuple2(stateSet, tmp)
            tmp
          })
        )
      }
    }

    val dfa = new DFA(transitTable.toArray(newStateId * 0x100), resultMap.to(TreeMap))
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
    val initialState: State = State(1)
    val jamState: Int = 0
  }

  private implicit def stateToInt(state: State): Int = state.value
}
