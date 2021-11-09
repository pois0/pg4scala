package jp.pois.pg4scala
package lexer

import lexer.Regex.Alternation.{EnumeratedAlternation, RangeAlternation}
import lexer.Utils.ASCII_SIZE

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private[lexer] class NFA(val table: mutable.Buffer[Map[Int, Seq[Int]]])

private[lexer] object NFA {
  val epsilon: Int = ASCII_SIZE

  def fromRegex(regex: Regex): NFA = {
    val innerTable = fromRegex(regex, 2, 1).table
    val buf = ArrayBuffer[Map[Int, Seq[Int]]](
      Map(epsilon -> Seq(2)),
      Map.empty
    )
    buf ++= innerTable
    new NFA(buf)
  }

  def fromRegex(regex: Regex, stateOffset: Int, finishState: Int): NFA = regex match {
    case Regex.Symbol(c) =>
      new NFA(
        mutable.Buffer(
          Map(c -> Seq(finishState)),
        )
      )
    case RangeAlternation(from, to) =>
      new NFA(
        mutable.Buffer(
          from.to(to).map(_ -> Seq(finishState)).toMap,
        )
      )
    case EnumeratedAlternation(es) => {
      val initialTrans = ArrayBuffer[Int]()
      val buf = mutable.UnrolledBuffer[Map[Int, Seq[Int]]]()
      var offSet = stateOffset + 1

      for (e <- es) {
        initialTrans += offSet
        val table = fromRegex(e, offSet, finishState).table
        offSet += table.size
        buf ++= table
      }

      buf.insert(0, Map(epsilon -> initialTrans))
      new NFA(buf)
    }
    case Regex.Concatenation(es) => {
      val buf = mutable.UnrolledBuffer[Map[Int, Seq[Int]]]()
      var offset = stateOffset + 1
      var finish = finishState

      for (e <- es.reverse) {
        val table = fromRegex(e, offset, finish).table
        finish = offset
        offset += table.size
        buf ++= table
      }

      buf.insert(0, Map(epsilon -> Seq(finish)))

      new NFA(buf)
    }
    case Regex.Repetition(e) =>
      new NFA(
        mutable.UnrolledBuffer[Map[Int, Seq[Int]]](
          Map(epsilon -> Seq(finishState, stateOffset + 1))
        ) ++= fromRegex(e, stateOffset + 1, stateOffset).table
      )
    case Regex.Epsilon =>
      new NFA(
        mutable.Buffer(
          Map(epsilon -> Seq(finishState))
        )
      )
    case Regex.Wildcard =>
      new NFA(
        mutable.Buffer(
          (0 until ASCII_SIZE).map(_ -> Seq(finishState)).toMap
        )
      )
  }
}
