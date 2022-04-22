package jp.pois.pg4scala
package lexer

import common.Token
import common.Token.{EOF, Skip}
import lexer.DFA.{State, TransitionResult}
import lexer.Lexer.isBreakChar
import lexer.MatchedContext.CharPosition
import lexer.Regex.Alternation.EnumeratedAlternation
import lexer.exceptions.MismatchedCharException

import java.io.{ByteArrayOutputStream, Reader, UnsupportedEncodingException}
import java.nio.charset.StandardCharsets
import scala.collection.mutable.ArrayBuffer

final class Lexer private[Lexer](private val dfa: DFA) {
  def lex(reader: Reader): LazyList[Token] = new Stepper(reader).loop(DFA.State.initialState).filterNot { _ eq Skip }

  private class Stepper(private val reader: Reader) {
    private val strBuf = new ByteArrayOutputStream()
    private var currentIndex = 0
    private var currentRow = 0
    private var currentColumn = 0
    private var broken = false
    private var startAt = CharPosition(currentRow, currentColumn, currentIndex)
    private val buf = new Array[Int](3)
    private var bufIndex = -1

    def loop(state: State, char: Int = read()): LazyList[Token] = {
      var currentState = state
      var c = char

      while (c >= 0) {
        dfa.transit(currentState, c) match {
          case TransitionResult.OnGoing(state) => {
            currentState = state
            c = pushAndRead(c)
          }
          case TransitionResult.Accepted(tokenGen) => return tokenGen(reset()) #:: loop(DFA.State.initialState, c)
          case TransitionResult.Rejected => {
            val context = reset()
            throw new MismatchedCharException(context.matchedString, context.start, context.end)
          }
        }
      }

      dfa.currentResult(currentState) match {
        case TransitionResult.Accepted(tokenGen) => {
          shift()
          tokenGen(reset()) #:: EOF #:: LazyList.empty
        }
        case TransitionResult.Rejected => {
          val context = reset()
          throw new MismatchedCharException(context.matchedString, context.start, context.end)
        }
        case TransitionResult.OnGoing(_) => throw new UnsupportedOperationException
      }
    }

    private def read(): Int = {
      val index = bufIndex
      if (index < 0) {
        val c = reader.read()

        if (c < 0x80) {
          c
        } else if (c < 0x800) {
          bufIndex = 0
          buf(0) = 0x80 | (c & 0x3f)
          0xc0 | (c >> 6)
        } else if (0xd800 <= c && c < 0xdc00) {
          val hi = c
          val lo = reader.read()
          val c32 = 0x10000 + (hi - 0xd800) * 0x400 + (lo - 0xdc00)
          bufIndex = 2
          buf(2) = 0x80 | ((c32 >> 12) & 0x3f)
          buf(1) = 0x80 | ((c32 >> 6) & 0x3f)
          buf(0) = 0x80 | (c32 & 0x3f)
          0xf0 | (c >> 18)
        } else if (c < 0x10000) {
          bufIndex = 1
          buf(1) = 0x80 | ((c >> 6) & 0x3f)
          buf(0) = 0x80 | (c & 0x3f)
          0xe0 | (c >> 12)
        } else {
          throw new UnsupportedEncodingException
        }
      } else {
        bufIndex = index - 1
        buf(index)
      }
    }

    @inline private def pushAndRead(prev: Int) = {
      strBuf.write(prev)
      if (bufIndex < 0) {
        currentIndex += 1
        currentColumn += 1
      }
      val c = read()
      if (broken) handleLineBreak()
      if (isBreakChar(c)) broken = true
      c
    }

    @inline private def shift(): Unit = {
      currentIndex += 1
      currentColumn += 1
    }

    @inline private def handleLineBreak(): Unit = {
      currentRow += 1
      currentColumn = -1
      broken = false
    }

    @inline private def currentCharPosition = CharPosition(currentRow, currentColumn, currentIndex)

    private def reset(): MatchedContext = {
      val startAtTmp = startAt
      val current = currentCharPosition
      val matchedString = strBuf.toString(StandardCharsets.UTF_8)
      startAt = if (broken) {
        handleLineBreak()
        currentCharPosition
      } else current
      strBuf.reset()
      MatchedContext(matchedString, startAtTmp, current)
    }
  }
}

object Lexer {
  type TokenGenerator = MatchedContext => Token

  private val skipGenerator: TokenGenerator = { _ => Skip }

  def builder = new LexerBuilder

  class LexerBuilder private[Lexer]() {
    private val rules = new ArrayBuffer[(Regex, TokenGenerator)]

    def build: Lexer = new Lexer(DFA.fromNFA(NFA.fromRegexes(rules.toSeq: _*)))

    def rule(regex: Regex, token: Token): LexerBuilder = {
      rules += Tuple2(regex, { _ => token})
      this
    }

    def rule(regex: Regex, mapToToken: TokenGenerator): LexerBuilder = {
      rules += Tuple2(regex, mapToToken)
      this
    }

    def ignore(regexes: Regex*): LexerBuilder = {
      rules += Tuple2(EnumeratedAlternation(regexes), skipGenerator)
      this
    }
  }

  private def isBreakChar(c: Int) = c == 0xA
}
