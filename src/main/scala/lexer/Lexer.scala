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
  def lex(reader: Reader): Stream[Token] = {
    val strBuf = StringBuilder.newBuilder
    var currentIndex = 0
    var currentRow = 0
    var currentColumn = 0
    var broken = false
    var startAt = CharPosition(currentRow, currentColumn, currentIndex)

    @inline def read() = reader.read()

    @inline def pushAndRead(prev: Int) = {
      strBuf.append(prev.toChar)
      val c = read()
      currentIndex += 1
      currentColumn += 1
      handleLineBreak()
      if (isBreakChar(c)) broken = true
      c
    }

    @inline def shift(): Unit = {
      currentIndex += 1
      currentColumn += 1
    }

    def handleLineBreak(): Unit = {
      if (broken) {
        currentRow += 1
        currentColumn = -1
        broken = false
      }
    }

    @inline def currentCharPosition = CharPosition(currentRow, currentColumn, currentIndex)

    def reset(): MatchedContext = {
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

    def loop(state: State, char: Int): Stream[Token] = {
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

    loop(DFA.State.initialState, read()).filterNot { _ eq Skip }
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
