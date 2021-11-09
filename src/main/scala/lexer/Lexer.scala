package jp.pois.pg4scala
package lexer

import common.Token
import lexer.DFA.{State, TransitionResult}
import lexer.exceptions.MismatchedCharException
import utils.Using

import scala.collection.mutable.ArrayBuffer
import scala.io.BufferedSource

class Lexer private[Lexer](private val dfa: DFA) {
  def lex(source: BufferedSource): Stream[Token] = {
    Using(source.bufferedReader()) { reader =>
      def loop(state: State, c: Int, strBuf: StringBuilder): Stream[Token] = {
        if (c < 0) {
          dfa.currentResult(state) match {
            case TransitionResult.Accepted(tokenGen) => tokenGen(strBuf.result) #:: Stream.empty
            case TransitionResult.Rejected => throw new MismatchedCharException
            case TransitionResult.OnGoing(_) => throw new UnsupportedOperationException
          }
        } else {
          dfa.transit(state, c) match {
            case TransitionResult.OnGoing(state) => loop(state, reader.read(), strBuf.append(c))
            case TransitionResult.Accepted(tokenGen) => tokenGen(strBuf.result) #:: loop(DFA.State.initialState, c, new StringBuilder)
            case TransitionResult.Rejected => throw new MismatchedCharException
          }
        }
      }

      loop(DFA.State.initialState, reader.read(), new StringBuilder)
    }
  }
}

object Lexer {
  def builder() = new LexerBuilder

  class LexerBuilder private[Lexer]() {
    private val rules = new ArrayBuffer[(Regex, String => Token)]

    def build: Lexer = ???

    def rule(regex: Regex, token: Token): LexerBuilder = {
      rules += (regex, { _ => token})
      this
    }

    def rule(regex: Regex, mapToToken: String => Token): LexerBuilder = {
      rules += Tuple2(regex, mapToToken)
      this
    }
  }
}
