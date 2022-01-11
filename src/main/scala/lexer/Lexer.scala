package jp.pois.pg4scala
package lexer

import common.Token
import common.Token.{EOF, Skip}
import lexer.DFA.{State, TransitionResult}
import lexer.Lexer.isBreakChar
import lexer.MatchedContext.CharPosition
import lexer.Regex.Alternation.EnumeratedAlternation
import lexer.exceptions.MismatchedCharException

import java.io.Reader
import scala.collection.mutable.ArrayBuffer

class Lexer private[Lexer](private val dfa: DFA) {
  def lex(reader: Reader): Stream[Token] = new Stepper(reader).loop(DFA.State.initialState).filterNot { _ eq Skip }

  private class Stepper(private val reader: Reader) {
    private val strBuf = StringBuilder.newBuilder
    private var currentIndex = 0
    private var currentRow = 0
    private var currentColumn = 0
    private var broken = false
    private var startAt = CharPosition(currentRow, currentColumn, currentIndex)

    def loop(state: State, char: Int = read()): Stream[Token] = {
      var currentState = state
      var c = char

      while (c >= 0) {
        dfa.transit(currentState, c) match {
          case TransitionResult.OnGoing(state) => {
            currentState = state
            c = pushAndRead(c)
          }
          case TransitionResult.Accepted(tokenGen) => return tokenGen(reset()) #:: loop(DFA.State.initialState, c)
          case TransitionResult.Rejected => throw new MismatchedCharException
        }
      }

      dfa.currentResult(currentState) match {
        case TransitionResult.Accepted(tokenGen) => {
          shift()
          tokenGen(reset()) #:: EOF #:: Stream.empty
        }
        case TransitionResult.Rejected => throw new MismatchedCharException
        case TransitionResult.OnGoing(_) => throw new UnsupportedOperationException
      }
    }

    @inline private def read() = reader.read()

    @inline private def pushAndRead(prev: Int) = {
      strBuf.append(prev.toChar)
      val c = read()
      currentIndex += 1
      currentColumn += 1
      if (broken) handleLineBreak()
      if (isBreakChar(c)) broken = true
      c
    }

    @inline private def shift(): Unit = {
      currentIndex += 1
      currentColumn += 1
    }

    private def handleLineBreak(): Unit = {
      currentRow += 1
      currentColumn = -1
      broken = false
    }

    @inline private def currentCharPosition = CharPosition(currentRow, currentColumn, currentIndex)

    private def reset(): MatchedContext = {
      val startAtTmp = startAt
      val current = currentCharPosition
      val matchedString = strBuf.result
      startAt = if (broken) {
        handleLineBreak()
        currentCharPosition
      } else current
      strBuf.clear
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

    def build: Lexer = new Lexer(DFA.fromNFA(NFA.fromRegexes(rules: _*)))

    def rule(regex: Regex, token: Token): LexerBuilder = {
      rules += Tuple2(regex, { _ => token})
      this
    }

    def rule(regex: Regex, mapToToken: TokenGenerator): LexerBuilder = {
      rules += Tuple2(regex, mapToToken)
      this
    }

    def ignore(regexes: Regex*): LexerBuilder = {
      rules += Tuple2(EnumeratedAlternation(regexes.toArray), skipGenerator)
      this
    }
  }

  private def isBreakChar(c: Int) = c == 0xA
}
