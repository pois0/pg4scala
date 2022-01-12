package jp.pois.pg4scala
package lexer

import lexer.Lexer.TokenGenerator
import lexer.NFA.Transit
import lexer.Regex.Alternation.{EnumeratedAlternation, RangeAlternation}
import lexer.Utils.ASCII_SIZE
import utils.NumericConstantMap

import java.util
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private[lexer] case class NFA(table: mutable.Buffer[Transit], initialState: Int, resultMap: Map[Int, TokenGenerator], resultOrder: Map[TokenGenerator, Int])

private[lexer] object NFA {
  type Transit = Map[Int, Seq[Int]]

  val epsilon: Int = -1

  def fromRegexes(regexes: (Regex, TokenGenerator)*): NFA = {
    val regSize = regexes.size
    val buf = new ArrayBuffer[Map[Int, Seq[Int]]](regSize * 2 + 1)
    for (_ <- regexes.indices) buf += Map.empty
    val arr = regexes.zipWithIndex.map { case ((_, gen), i) => i -> gen }.toMap

    def loop(regex: Regex, stateOffset: Int, finishState: Int): Int = regex match {
      case Regex.Symbol(c) => {
        buf += Map(c -> Seq(finishState))
        1
      }
      case RangeAlternation(range) => {
        buf += new NumericConstantMap[Seq[Int]](range, Seq(finishState))
        1
      }
      case EnumeratedAlternation(es) => {
        val initialIndex = buf.size
        val initialTrans = ArrayBuffer[Int]()
        buf += Map(epsilon -> initialTrans)
        var offSet = stateOffset + 1

        for (e <- es) {
          initialTrans += offSet
          offSet += loop(e, offSet, finishState)
        }

        offSet - initialIndex
      }
      case Regex.Concatenation(es) => {
        val initialIndex = buf.size
        val initSeq = mutable.Seq(0)
        buf += Map(epsilon -> initSeq)
        var offset = stateOffset + 1
        var finish = finishState

        for (e <- es.reverse) {
          val size = loop(e, offset, finish)
          finish = offset
          offset += size
        }

        initSeq(0) = finish
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
        buf += new NumericConstantMap(0 until ASCII_SIZE, Seq(finishState))
        1
      }
    }

    var offset = regSize + 1
    val seq = ArrayBuffer[Int]()
    buf += Map(epsilon -> seq)

    for (((regex, _), i) <- regexes.zipWithIndex) {
      seq += offset
      offset += loop(regex, offset, i)
    }

    eliminateEpsilon(new NFA(buf, regSize, arr, regexes.zipWithIndex.reverse.map { case ((_, gen), i) => gen -> i }.toMap))
  }

  def eliminateEpsilon(nfa: NFA): NFA = {
    val table = nfa.table
    val size = table.size
    val visited = new util.BitSet(size)
    var firstClearBit = 0
    val resultMap = mutable.Map.empty ++ nfa.resultMap

    def loop(index: Int): Transit = {
      val transit = table(index)

      if (visited.get(index)) return transit
      visited.set(index)

      transit.get(epsilon).map { epsilonSeq =>
        val tmp = mutable.Map() ++ transit
        var tmpTransition: Option[TokenGenerator] = None
        var tmpRank =  Int.MaxValue
        tmp.remove(epsilon)
        for (s <- epsilonSeq) {
          for ((c, seq) <- loop(s) if c != epsilon) {
            tmp(c) = tmp.get(c)
              .map { _ ++ seq }
              .getOrElse(seq)
          }
          resultMap.get(s).foreach { transit =>
            val rank = nfa.resultOrder(transit)
            if (rank < tmpRank) {
              tmpRank = rank
              tmpTransition = Some(transit)
            }
          }
        }
        val res = tmp.toMap
        table(index) = res
        tmpTransition.foreach { resultMap(index) = _ }
        res
      }.getOrElse(transit)
    }

    while ({ firstClearBit = visited.nextClearBit(firstClearBit); firstClearBit < size }) {
      loop(firstClearBit)
    }

    new NFA(table, nfa.initialState, resultMap.toMap, nfa.resultOrder)
  }
}
