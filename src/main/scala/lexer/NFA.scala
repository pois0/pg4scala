package jp.pois.pg4scala
package lexer

import lexer.NFA.Transit
import lexer.Regex.Alternation.{EnumeratedAlternation, RangeAlternation}
import lexer.Utils.ASCII_SIZE

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private[lexer] class NFA(val table: mutable.Buffer[Transit])

private[lexer] object NFA {
  private type Transit = Map[Int, Seq[Int]]

  val epsilon: Int = ASCII_SIZE
  val initialState = 1
  val finishState = 0

  def fromRegex(regex: Regex): NFA = {
    val buf = ArrayBuffer[Transit](Map.empty)

    def loop(regex: Regex, stateOffset: Int, finishState: Int): Int = regex match {
      case Regex.Symbol(c) => {
        buf += Map(c -> Seq(finishState))
        1
      }
      case RangeAlternation(from, to) => {
        buf += from.to(to).map(_ -> Seq(finishState)).toMap
        1
      }
      case EnumeratedAlternation(es) => {
        val initialIndex = buf.size
        buf += Map.empty
        val initialTrans = ArrayBuffer[Int]()
        var offSet = stateOffset + 1

        for (e <- es) {
          initialTrans += offSet
          offSet += loop(e, offSet, finishState)
        }

        buf(initialIndex) = Map(epsilon -> initialTrans)
        offSet - initialIndex
      }
      case Regex.Concatenation(es) => {
        val initialIndex = buf.size
        buf += Map.empty
        var offset = stateOffset + 1
        var finish = finishState

        for (e <- es.reverse) {
          val size = loop(e, offset, finish)
          finish = offset
          offset += size
        }

        buf(initialIndex) = Map(epsilon -> Seq(finish))
        offset - initialIndex
      }
      case Regex.Repetition(e) => {
        buf += Map(epsilon -> Seq(finishState, stateOffset + 1))
        loop(e, stateOffset + 1, stateOffset) + 1
      }
      case Regex.Epsilon => {
        buf += Map(epsilon -> Seq(finishState))
        1
      }
      case Regex.Wildcard => {
        buf += (0 until ASCII_SIZE).map(_ -> Seq(finishState)).toMap
        1
      }
    }

    loop(regex, 1, 0)
    new NFA(buf)
  }
}
