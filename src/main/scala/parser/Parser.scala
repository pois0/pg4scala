package jp.pois.pg4scala
package parser

import parser.Parser.AnalyseResult.{Reduce, Shift}
import parser.Parser.ParserBuilder.{ParserBuilderConstraints, Set, Unset}
import parser.Parser.ParserStackTerm.{ParsedValue, Token}
import parser.Parser.{AnalyseResult, ParseResult, ParserStackElement, ParserStackTerm}
import parser.Term.NonTerminalSymbol
import utils.{ConstantMap, RawArrayBuffer}

import jp.pois.pg4scala.parser.Parser.ParseResult.Value
import jp.pois.pg4scala.parser.Parser.ParserStackElement.Bottom

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Parser[Value] private(
  private val termMap: Array[Map[Class[_], AnalyseResult[Value]]],
  private val nonTermMap: Array[Map[NonTerminalSymbol, Int]]
) {
  def parse(tokens: Stream[common.Token]): Value = {
    val stack = new mutable.ArrayStack[ParserStackElement[Value]]
    stack += Bottom
    var currentState = 0

    def step(token: common.Token): Option[Value] = termMap(currentState).get(token.getClass).flatMap {
      _ match {
        case AnalyseResult.Accept => stack.pop().term match {
          case ParsedValue(_, value) => Some(value)
          case Token(_) => ??? // throw Error
        }
        case AnalyseResult.Shift(goto) => {
          println(s"Shift / Token: $token, current: $currentState, goto: $goto")
          stack.push(ParserStackElement.Element(ParserStackTerm.Token(token), goto))
          currentState = goto
          None
        }
        case AnalyseResult.Reduce(symbol: NonTerminalSymbol, numberOfRight, generator) => {
          println(s"Reduce / Token: $token, current: $currentState")
          val arr = new Array[ParseResult[Value]](numberOfRight)
          for (i <- arr.indices.reverse) {
            arr(i) = stack.pop().term.toResult
          }
          currentState = nonTermMap(stack.top.state)(symbol)
          stack.push(ParserStackElement.Element(ParsedValue(symbol, generator(arr.toSeq).asInstanceOf[Value]), currentState))
          step(token) // TODO Remove cast
        }
      }
    }

    tokens.flatMap(step).headOption match {
      case Some(value) => value
      case None => ??? // Error handling
    }
  }
}

object Parser {
  type ResultGenerator[Value] = Seq[ParseResult[Value]] => Value

  def builder[Value]: ParserBuilder[Value, Unset] = new ParserBuilder

  class ParserBuilder[Value, Constraints <: ParserBuilderConstraints] private[Parser]
  (
    private val initialTerm: Option[NonTerminalSymbol] = None,
    private val grammar: mutable.Map[NonTerminalSymbol, ArrayBuffer[(Array[Term], ResultGenerator[Value])]] =
      mutable.Map.empty[NonTerminalSymbol, ArrayBuffer[(Array[Term], ResultGenerator[Value])]]
  ) {
    type TermWithDot = (NonTerminalSymbol, Array[Term], Int, ResultGenerator[Value])
    type TermSet = mutable.Set[TermWithDot]

    def build(implicit INITIAL_NON_TERMINAL_SET: Constraints =:= Set): Parser[Value] = {
      def closure(terms: TermSet): TermSet = {
        val stack = mutable.Stack.newBuilder.++=(terms).result()

        while (stack.nonEmpty) {
          val (_, right, currentIndex, _) = stack.pop()
          if (currentIndex < right.length) {
            right(currentIndex) match {
              case Term.NonTerminal(tmp) => {
                grammar.getOrElse(tmp, Nil).foreach { case (tmpRight, generator) =>
                  val candidate = (tmp, tmpRight, 0, generator)
                  if (!terms.contains(candidate)) {
                    stack.push(candidate)
                    terms += candidate
                  }
                }
              }
              case Term.Terminal(_) =>
            }
          }
        }

        terms
      }

      def goto(terms: TermSet, symbol: Term): TermSet = {
        val tmp: TermSet = mutable.Set()
        for ((left, right, currentIndex, generator) <- terms if currentIndex <= right.length && right(currentIndex) == symbol) {
          tmp += Tuple4(left, right, currentIndex + 1, generator)
        }
        closure(tmp)
      }

      val initialState = closure(
        mutable.Set(
          Tuple4(NonTerminalSymbol.SpecialNonTerminal, Array(Term.NonTerminal(initialTerm.get), Term.Terminal[common.Token.EOF.type]), 0, { arr =>
            arr.head match {
              case ParseResult.Value(value) => value
              case _ => throw new IllegalStateException()
            } })
        )
      )

      val stateMap = mutable.Map(initialState -> 0)
      val reduceStates = mutable.Map[Int, (NonTerminalSymbol, Int, ResultGenerator[Value])]()

      val termMap = new RawArrayBuffer[mutable.Map[Class[_], AnalyseResult[Value]]](1)
      val nonTermMap = new RawArrayBuffer[mutable.Map[NonTerminalSymbol, Int]](1)

      termMap(0) = mutable.Map()
      nonTermMap(0) = mutable.Map()
      val stack = mutable.ArrayStack[(Int, TermSet)]()

      stack.push((0, initialState))
      while (stack.nonEmpty) {
        val (state, term) = stack.pop()
        for ((left, right, currentIndex, generator) <- term) {
          if (currentIndex < right.length) {
            val currentToken = right(currentIndex)
            if (currentToken == Term.Terminal.EOF) {
              termMap(state)(common.Token.EOF.getClass) = AnalyseResult.Accept
            } else {
              val gt = goto(term, currentToken)
              val gtState = stateMap.getOrElseUpdate(gt, {
                val newIndex = stateMap.size
                stack.push((newIndex, gt))
                termMap(newIndex) = mutable.Map()
                nonTermMap(newIndex) = mutable.Map()
                newIndex
              })

              currentToken match {
                case Term.Terminal(clazz) => termMap(state)(clazz) = Shift(gtState)
                case Term.NonTerminal(symbol) => nonTermMap(state)(symbol) = gtState
              }
            }
          } else {
            if (reduceStates.contains(state)) {
              throw new IllegalStateException("Reduce / Reduce conflict")
            }
            reduceStates(state) = (left, right.length, generator)
          }
        }
      }

      val tm = new Array[Map[Class[_], AnalyseResult[Value]]](termMap.size)

      println(stateMap.map { case (set, state) => (state, "$state:\n\t" + set.map { case (symbol, terms, i, generator) => s"($symbol -> ${terms.mkString(",")}, $i)" }.mkString(",\n\t")) }.toSeq.sortBy { case (i, str) => i }.mkString("\n"))

      for (i <- termMap.indices) {
        tm(i) = reduceStates.get(i)
          .map { case (sym, num, generator) =>
            val reduce = Reduce(sym, num, generator)
            println(i, reduce)
            new ConstantMap[Class[_], AnalyseResult[Value]](reduce) }
          .getOrElse {
            val m = termMap(i).toMap; println(i, m); m }
      }

      val ntm = new Array[Map[NonTerminalSymbol, Int]](nonTermMap.length)
      for (i <- ntm.indices) {
        ntm(i) = nonTermMap(i).toMap
        println(i, ntm(i))
      }

      new Parser[Value](tm, ntm)
    }

    def initialTerm(symbol: NonTerminalSymbol): ParserBuilder[Value, Set] =
      new ParserBuilder[Value, Set](Some(symbol), grammar)

    def rule(left: NonTerminalSymbol, right: Array[Term], mapToValue: ResultGenerator[Value]): ParserBuilder[Value, Constraints] = {
      val tup = Tuple2(right, mapToValue)
      grammar(left) = grammar.get(left).map { buf => buf += tup }.getOrElse(ArrayBuffer(tup))
      this
    }
  }

  object ParserBuilder {
    private[Parser] sealed trait ParserBuilderConstraints

    private[Parser] trait Set extends ParserBuilderConstraints
    private[Parser] trait Unset extends ParserBuilderConstraints
  }

  sealed abstract class ParseResult[+Value] {
    def asToken: common.Token
    def asValue: Value
  }

  object ParseResult {
    final case class Token[+Value] private[parser](token: common.Token) extends ParseResult[Value] {
      override def asToken: common.Token = token
      override def asValue: Value = ???
    }
    final case class Value[+Value] private[parser](value: Value) extends ParseResult[Value] {
      override def asToken: common.Token = ???
      override def asValue: Value = value
    }
  }

  private[parser] sealed abstract class AnalyseResult[-Value]

  private[parser] object AnalyseResult {
    case object Accept extends AnalyseResult[Any]
    final case class Shift(goto: Int) extends AnalyseResult[Any]
    final case class Reduce[Value](symbol: NonTerminalSymbol, numberOfRight: Int, generator: ResultGenerator[Value]) extends AnalyseResult[Value]
  }

  private[parser] sealed abstract class ParserStackElement[+Value] {
    def term: ParserStackTerm[Value]
    def state: Int
  }

  object ParserStackElement {
    private[parser] case class Element[Value](term: ParserStackTerm[Value], state: Int) extends ParserStackElement[Value]
    private[parser] case object Bottom extends ParserStackElement[Nothing] {
      override def term: ParserStackTerm[Nothing] = throw new IllegalStateException()
      override def state: Int = 0
    }
  }

  private[parser] sealed abstract class ParserStackTerm[+Value] {
    def toResult: ParseResult[Value]
  }

  private[parser] object ParserStackTerm {
    final case class Token[+Value] private[parser](token: common.Token) extends ParserStackTerm[Value] {
      override def toResult: ParseResult[Value] = ParseResult.Token(token)
    }
    final case class ParsedValue[+Value] private[parser](sym: NonTerminalSymbol, value: Value) extends ParserStackTerm[Value] {
      override def toResult: ParseResult[Value] = ParseResult.Value(value)
    }
  }
}
