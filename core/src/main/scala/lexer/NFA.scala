package jp.pois.pg4scala
package lexer

import lexer.Lexer.TokenGenerator
import lexer.NFA.Transit
import lexer.Regex.Alternation.{CustomFilterAlternation, EnumeratedAlternation}
import lexer.util.{ArrayMap, ArrayPairMap, IntMap}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private[lexer] final case class NFA(table: mutable.Buffer[Transit], initialState: Int, resultMap: Map[Int, TokenGenerator], resultOrder: Map[TokenGenerator, Int])

private[lexer] object NFA {
  type Transit = Map[Int, Seq[Int]]

  val epsilon: Int = -1

  def fromRegexes(regexes: (Regex, TokenGenerator)*): NFA = {
    val regSize = regexes.size
    val buf = new ArrayBuffer[Map[Int, Seq[Int]]](regSize * 2 + 1)
    for (_ <- regexes.indices) buf += Map.empty
    val arr = regexes.zipWithIndex.map { case ((_, gen), i) => i -> gen }.toMap

    def loop(regex: Regex, stateOffset: Int, finishState: Int): Int = regex match {
      case Regex.Symbol8(c) => {
        buf += Map((c & 0xff) -> Seq(finishState))
        1
      }
      case Regex.Symbol32(c) => {
        if (c < 0) {
          throw new IllegalArgumentException
        } else if (c < 0x80) {
          buf += Map(c -> Seq(finishState))
          1
        } else if (c < 0x800) {
          val c1 = 0xc0 | (c >> 6)
          val c2 = 0x80 | (c & 0x3f)
          buf += Map(c1 -> Seq(stateOffset + 1))
          buf += Map(c2 -> Seq(finishState))
          2
        } else if (c < 0x10000) {
          val c1 = 0xe0 | (c >> 12)
          val c2 = 0x80 | ((c >> 6) & 0x3f)
          val c3 = 0x80 | (c & 0x3f)
          buf += Map(c1 -> Seq(stateOffset + 1))
          buf += Map(c2 -> Seq(stateOffset + 2))
          buf += Map(c3 -> Seq(finishState))
          3
        } else if (c < 0x110000) {
          val c1 = 0xf0 | (c >> 18)
          val c2 = 0x80 | ((c >> 12) & 0x3f)
          val c3 = 0x80 | ((c >> 6) & 0x3f)
          val c4 = 0x80 | (c & 0x3f)
          buf += Map(c1 -> Seq(stateOffset + 1))
          buf += Map(c2 -> Seq(stateOffset + 2))
          buf += Map(c3 -> Seq(stateOffset + 3))
          buf += Map(c4 -> Seq(finishState))
          4
        } else {
          throw new IllegalArgumentException
        }
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
      case CustomFilterAlternation(iterable) => {
        val iter = iterable.iterator
        if (iter.isEmpty) {
          buf += Map.empty
          return 1
        }

        var c = iter.next()
        var offset = stateOffset + 4
        val fs = Seq(finishState)
        val f1s = Seq(stateOffset + 1)
        val f2s = Seq(stateOffset + 2)
        val f3s = Seq(stateOffset + 3)


        val rootDomainBuf = new ArrayBuffer[Int]()
        val rootRangeBuf = new ArrayBuffer[Seq[Int]]()

        buf += new ArrayPairMap(rootDomainBuf, rootRangeBuf)
        buf += new WildCardMapRest(fs)
        buf += new WildCardMapRest(f1s)
        buf += new WildCardMapRest(f2s)

        @inline
        def leave: Int = offset - stateOffset

        while (c < 0x80) {
          rootDomainBuf.append(c)
          rootRangeBuf.append(fs)

          if (iter.isEmpty) {
            buf(stateOffset) = new ArrayMap(rootDomainBuf, fs)
            return leave
          }
          c = iter.next()
        }

        var prev1 = -1
        var secDomainBuf = new ArrayBuffer[Int]

        @inline
        def clear1(): Unit = {
          if (prev1 != -1) {
            if (secDomainBuf.length == 0x40) {
              rootDomainBuf += prev1
              rootRangeBuf += f1s
            } else {
              rootDomainBuf += prev1
              rootRangeBuf += Seq(offset)
              buf += new ArrayMap(secDomainBuf, fs)

              offset += 1
              secDomainBuf = new ArrayBuffer
            }
          }
        }

        while (c < 0x800) {
          val c1 = 0xc0 | (c >> 6)
          val c2 = 0x80 | (c & 0x3f)

          if (prev1 != c1) clear1()
          prev1 = c1
          secDomainBuf += c2

          if (iter.isEmpty) {
            clear1()
            return leave
          }

          c = iter.next()
        }

        clear1()

        prev1 = -1
        var prev2 = -1
        var secRangeBuf = new ArrayBuffer[Seq[Int]]
        var trdDomainBuf = new ArrayBuffer[Int]

        @inline
        def clear2(): Unit = {
          if (prev2 != -1) {
            secDomainBuf += prev2
            secRangeBuf += Seq(offset)
            buf += new ArrayMap(trdDomainBuf, fs)

            offset += 1
            trdDomainBuf = new ArrayBuffer
          }
        }

        @inline
        def clear1n(): Unit = {
          if (prev1 != -1) {
            rootDomainBuf += prev1
            rootRangeBuf += Seq(offset)
            buf += new ArrayPairMap(secDomainBuf, secRangeBuf)

            offset += 1
            secDomainBuf = new ArrayBuffer
            secRangeBuf = new ArrayBuffer
          }
        }

        while (c < 0x10000) {
          val c1 = 0xe0 | (c >> 12)
          val c2 = 0x80 | ((c >> 6) & 0x3f)
          val c3 = 0x80 | (c & 0x3f)

          if (prev2 != c2) clear2()
          prev2 = c2
          if (prev1 != c1) clear1n()
          prev1 = c1
          trdDomainBuf += c3

          if (iter.isEmpty) {
            clear2()
            clear1n()
            return leave
          }

          c = iter.next()
        }

        clear2()
        clear1n()

        prev1 = -1
        prev2 = -1
        var prev3 = -1
        var trdRangeBuf = new ArrayBuffer[Seq[Int]]
        var fthDomainBuf = new ArrayBuffer[Int]

        @inline
        def clear3(): Unit = {
          if (prev3 != -1) {
            trdDomainBuf += prev3
            trdRangeBuf += Seq(offset)
            buf += new ArrayMap(fthDomainBuf, fs)

            offset += 1
            fthDomainBuf = new ArrayBuffer
          }
        }

        @inline
        def clear2n(): Unit = {
          if (prev2 != -1) {
            secDomainBuf += prev2
            secRangeBuf += Seq(offset)
            buf += new ArrayPairMap(trdDomainBuf, trdRangeBuf)

            offset += 1
            trdDomainBuf = new ArrayBuffer
            trdRangeBuf = new ArrayBuffer
          }
        }

        while (c < 0x110000) {
          val c1 = 0xf0 | (c >> 18)
          val c2 = 0x80 | ((c >> 12) & 0x3f)
          val c3 = 0x80 | ((c >> 6) & 0x3f)
          val c4 = 0x80 | (c & 0x3f)

          if (prev3 != c3) clear3()
          prev3 = c3
          if (prev2 != c2 && prev2 != -1) clear2n()
          prev2 = c2
          if (prev1 != c1) clear1n()
          prev1 = c1
          fthDomainBuf += c4

          if (iter.isEmpty) {
            clear3()
            clear2n()
            clear1n()
            return leave
          }

          c = iter.next()
        }

        clear3()
        clear2n()
        clear1n()

        leave
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
        val fs = Seq(finishState)
        val s1 = Seq(stateOffset + 1)
        val s2 = Seq(stateOffset + 2)
        val s3 = Seq(stateOffset + 3)
        buf += new WildcardMapHead(fs, s1, s2, s3)
        buf += new WildCardMapRest(fs)
        buf += new WildCardMapRest(s1)
        buf += new WildCardMapRest(s2)
        4
      }
    }

    var offset = regSize + 1
    val seq = new Array[Int](regSize)
    buf += Map(epsilon -> seq)

    for (((regex, _), i) <- regexes.zipWithIndex) {
      seq(i) = offset
      offset += loop(regex, offset, i)
    }

    eliminateEpsilon(new NFA(buf, regSize, arr, regexes.zipWithIndex.reverse.map { case ((_, gen), i) => gen -> i }.toMap))
  }

  def eliminateEpsilon(nfa: NFA): NFA = {
    val table = nfa.table
    val size = table.size
    val visited = new java.util.BitSet(size)
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

  private class WildcardMapHead[+V](private val finishState: V,
                             private val state2: V,
                             private val state3: V,
                             private val state4: V) extends IntMap[V] {
    override def get(key: Int): Option[V] = {
      if (key < 0) {
        None
      } else if (key < 0x80) {
        Some(finishState)
      } else if (key < 0xc2) {
        None
      } else if (key < 0xe0) {
        Some(state2)
      } else if (key < 0xf0) {
        Some(state3)
      } else if (key < 0xf5) {
        Some(state4)
      } else {
        None
      }
    }
  }

  private class WildCardMapRest[+V](private val nextState: V) extends IntMap[V] {
    override def iterator: Iterator[(Int, V)] = (0x80 until 0xc0).map { (_, nextState) }.iterator

    override def get(key: Int): Option[V] = if (0x80 <= key && key < 0xc0) Some(nextState) else None
  }

  private class RangeMap[+V](private val nextState: V, private val range: Range) extends IntMap[V] {
    override def iterator: Iterator[(Int, V)] = range.map { (_, nextState) }.iterator

    override def get(key: Int): Option[V] = if (range.contains(key)) Some(nextState) else None
  }
}
