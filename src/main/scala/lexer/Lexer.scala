package jp.pois.pg4scala
package lexer

import common.{Skip, Token}
import lexer.DFA.{State, TransitionResult}
import lexer.Regex.Alternation.EnumeratedAlternation
import lexer.exceptions.MismatchedCharException

import java.io.Reader
import scala.collection.mutable.ArrayBuffer

class Lexer private[Lexer](private val dfa: DFA) {
  def lex(reader: Reader): Stream[Token] = {
    def loop(state: State, c: Int, strBuf: StringBuilder): Stream[Token] = {
      if (c < 0) {
        dfa.currentResult(state) match {
          case TransitionResult.Accepted(tokenGen) => tokenGen(strBuf.result) #:: Stream.empty
          case TransitionResult.Rejected => throw new MismatchedCharException
          case TransitionResult.OnGoing(_) => throw new UnsupportedOperationException
        }
      } else {
        dfa.transit(state, c) match {
          case TransitionResult.OnGoing(state) => loop(state, reader.read(), strBuf.append(c.toChar))
          case TransitionResult.Accepted(tokenGen) => {
            val token = tokenGen(strBuf.result)
            if (token eq Skip) loop(DFA.State.initialState, c, new StringBuilder)
            else token #:: loop(DFA.State.initialState, c, new StringBuilder)
          }
          case TransitionResult.Rejected => throw new MismatchedCharException
        }
      }
    }

    loop(DFA.State.initialState, reader.read(), new StringBuilder)
  }
}

object Lexer {
  type TokenGenerator = String => Token

  private val skipGenerator: TokenGenerator = { _ => Skip }

  def builder() = new LexerBuilder

  class LexerBuilder private[Lexer]() {
    private val rules = new ArrayBuffer[(Regex, TokenGenerator)]

    def build: Lexer = new Lexer(DFA.fromNFA(NFA.fromRegexes(rules: _*)))

    def rule(regex: Regex, token: Token): LexerBuilder = {
      rules += Tuple2(regex, { _ => token})
      this
    }

    def rule(regex: Regex, mapToToken: String => Token): LexerBuilder = {
      rules += Tuple2(regex, mapToToken)
      this
    }

    def ignore(regexes: Regex*): LexerBuilder = {
      rules += Tuple2(EnumeratedAlternation(regexes.toArray), skipGenerator)
      this
    }
  }
}
