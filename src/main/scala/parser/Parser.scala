package jp.pois.pg4scala
package parser

import parser.Parser.AnalyseResult.{Accept, Reduce, Shift}
import parser.Parser.ParserBuilder.{LR0StateItem, LR1StateItem}
import parser.Parser.ParserStackElement.Bottom
import parser.Parser.ParserStackTerm.{ParsedValue, Token}
import parser.Parser.{AnalyseResult, ParseResult, ParserStackElement, ParserStackTerm}
import parser.Term.NonTerminalSymbol.SpecialNonTerminal
import parser.Term.Terminal.{EOF, EOFType, Sharp, SharpClass}
import parser.Term.{NonTerminal, NonTerminalSymbol, Terminal, TokenType, nonTerminalSymbolToTerm}

import java.util
import scala.Function.unlift
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

final class Parser[Value] private(
  private val termMap: Array[Map[Class[_], AnalyseResult[Value]]],
  private val nonTermMap: Array[Map[NonTerminalSymbol, Int]]
) {
  def parse(tokens: Stream[common.Token]): Value = tokens.flatMap((new Stepper).step).headOption match {
    case Some(value) => value
    case None => ??? // Error handling
  }

  private final class Stepper {
    val stack = new mutable.ArrayStack[ParserStackElement[Value]]
    stack += Bottom
    var currentState = 0

    @tailrec
    def step(token: common.Token): Option[Value] = termMap(currentState).get(token.getClass) match {
      case Some(res) => res match {
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
          val goto = nonTermMap(stack.top.state)(symbol)
          currentState = goto
          stack.push(ParserStackElement.Element(ParsedValue(symbol, generator(arr.toSeq).asInstanceOf[Value]), goto))
          step(token)
        }
      }
      case None => {
        println(s"Error / Token: $token, current: $currentState, table: ${termMap(currentState)}")
        ???
      }
    }
  }
}

object Parser {
  type ResultGenerator[Value] = Seq[ParseResult[Value]] => Value

  def builder[Value](initialTerm: NonTerminalSymbol): ParserBuilder[Value] = new ParserBuilder(initialTerm)

  final class ParserBuilder[Value] private[Parser](private val initialTerm: NonTerminalSymbol) {
    private val grammar = mutable.Map.empty[NonTerminalSymbol, ArrayBuffer[(Array[Term], ResultGenerator[Value])]]
    private val first = mutable.Map[Term, mutable.Set[TokenType]]()
    private val nullable = mutable.Set[Term]()
    type LR0StateItem = ParserBuilder.LR0StateItem[Value]
    type LR1StateItem = ParserBuilder.LR1StateItem[Value]
    type LR0ItemSet = mutable.Set[LR0StateItem]
    type LR1ItemSet = mutable.Set[LR1StateItem]

    private lazy val initialItem: LR0StateItem = LR0StateItem(NonTerminalSymbol.SpecialNonTerminal, Array(Term.NonTerminal(initialTerm)), 0, lastReduceRule)
    private lazy val initialState = lr0closure(mutable.Set(initialItem))

    def rule(left: NonTerminalSymbol, right: Array[Term], mapToValue: ResultGenerator[Value]): ParserBuilder[Value] = {
      val tup = Tuple2(right, mapToValue)
      grammar(left) = grammar.get(left).map { buf => buf += tup }.getOrElse(ArrayBuffer(tup))
      for (c <- right) {
        c match {
          case Terminal(tokenType) => first.getOrElseUpdate(c, { mutable.Set(tokenType) })
          case NonTerminal(_) =>
        }
      }

      this
    }

    def build: Parser[Value] = {
      constructFirstAndNull()
      val (lr0State, gotoTable) = generateLR0State
      val lr0Kernel = calcLR0Kernel(lr0State)
      val lalr1State = calcLALR1State(lr0Kernel, gotoTable)
      constructLR1Table(lalr1State)
    }

    private def constructFirstAndNull(): Unit = {
      var flag = true
      while(flag) {
        flag = false
        for ((left, rightList) <- grammar) {
          val leftTerm = NonTerminal(left)

          for ((right, _) <- rightList) {
            if ((!nullable.contains(leftTerm)) && right.forall(nullable.contains)) {
              nullable += leftTerm
              flag = true
            }
            var i = 0
            while (i < right.length) {
              if (i == 0 || nullable(right(i - 1))) {
                val c = right(i)
                val cFirst = first.getOrElseUpdate(c, mutable.Set.empty)
                val leftTermFirst = first.getOrElseUpdate(leftTerm, mutable.Set.empty)
                if (!cFirst.subsetOf(leftTermFirst)) {
                  leftTermFirst ++= cFirst
                  flag = true
                }
                i += 1
              } else {
                i = Int.MaxValue
              }
            }
          }
        }
      }

      first(Sharp) = mutable.Set(SharpClass)
      first(EOF) = mutable.Set(EOFType)
    }

    private def generateLR0State: (mutable.Map[LR0ItemSet, Int], mutable.Set[(Int, Int)]) = {
      val stateMap = mutable.Map(initialState -> 0)
      val stack = mutable.ArrayStack[(Int, LR0ItemSet)]()
      val gotoTable = mutable.Set.empty[(Int, Int)]

      stack.push((0, initialState))
      while (stack.nonEmpty) {
        val (currentState, term) = stack.pop()
        for (LR0StateItem(_, right, currentIndex, _) <- term) {
          if (currentIndex < right.length) {
            val currentToken = right(currentIndex)
            if (currentToken == Term.Terminal.EOF) {
            } else {
              val gt = lr0goto(term, currentToken)
              val goto = stateMap.getOrElseUpdate(gt, {
                val newIndex = stateMap.size
                stack.push((newIndex, gt))
                newIndex
              })
              gotoTable += currentState -> goto
            }
          }
        }
      }

      (stateMap, gotoTable)
    }

    private def calcLR0Kernel(stateMap: mutable.Map[LR0ItemSet, Int]): mutable.Map[LR0ItemSet, Int] = {
      val kernel: mutable.Map[LR0ItemSet, Int] = stateMap.map { case (itemSet, state) =>
        itemSet.filter { case LR0StateItem(left, _, i, _) => i > 0 || left == SpecialNonTerminal } -> state
      }
      kernel
    }

    private def constructLR1Table(states: mutable.Map[LR1ItemSet, Int]): Parser[Value] = {
      val test = states.collectFirst(unlift { case (set,i) => if (set.exists { case LR1StateItem(LR0StateItem(k, _, i, _), tokenType) => k == SpecialNonTerminal && i == 1 }) Some((set,i)) else None }).get
      val termMap = ArrayBuffer.fill[mutable.Map[Class[_], AnalyseResult[Value]]](states.size) { mutable.Map.empty }
      val nonTermMap = ArrayBuffer.fill[mutable.Map[NonTerminalSymbol, Int]](states.size) { mutable.Map.empty }

      val stack = new mutable.ArrayStack[(LR1ItemSet, Int)]
      stack ++= states

      while (stack.nonEmpty) {
        val (set, state) = stack.pop()
        for (LR1StateItem(LR0StateItem(left, right, rightIndex, generator), la) <- set) {
          if (rightIndex < right.length) {
            val current = right(rightIndex)
            val g = lr1goto(set, current)
            val goto = states.getOrElseUpdate(g, {
              val newIndex = termMap.size
              stack.push((g, newIndex))
              termMap.append(mutable.Map.empty)
              nonTermMap.append(mutable.Map.empty)
              newIndex
            })
            current match {
              case Terminal(clazz) => termMap(state)(clazz) = Shift(goto)
              case NonTerminal(symbol) => nonTermMap(state)(symbol) = goto
            }
          } else {
            if (left == NonTerminalSymbol.SpecialNonTerminal) {
              if (la == EOFType) {
                termMap(state)(EOFType) = Accept
              } else {
                ??? // error
              }
            } else {
              termMap(state)(la) = Reduce(left, right.length, generator)
            }
          }
        }
      }

      val tm = new Array[Map[Class[_], AnalyseResult[Value]]](termMap.size)
      val ntm = new Array[Map[NonTerminalSymbol, Int]](nonTermMap.size)

      for (i <- termMap.indices) {
        tm(i) = termMap(i).toMap
        ntm(i) = nonTermMap(i).toMap
      }

      new Parser(tm, ntm)
    }

    private def calcLALR1State(kernels: mutable.Map[LR0ItemSet, Int], gotoTable: mutable.Set[(Int, Int)]): mutable.Map[LR1ItemSet, Int] = {
      type ItemAndState = (Int, LR0StateItem)
      val propagation = mutable.Map.empty[ItemAndState, mutable.Set[ItemAndState]]
      val stack = new mutable.ArrayStack[(ItemAndState, mutable.Set[TokenType])]
      val result = mutable.Map.empty[Int, mutable.Map[LR0StateItem, mutable.Set[TokenType]]]

      stack.push(((0, initialItem), mutable.Set(EOFType)))
      result.put(0, mutable.Map(
        LR0StateItem(NonTerminalSymbol.SpecialNonTerminal, Array[Term](Term.NonTerminal(initialTerm)), 0, { lastReduceRule }) -> mutable.Set[Class[_]](EOFType)
      ))

      for ((kernel, state) <- kernels;
           kItem <- kernel;
           be <- lr1closure(mutable.Set(LR1StateItem(kItem, SharpClass)))) {
        val LR1StateItem(LR0StateItem(left, right, rightIndex, _), la) = be
        kernels.collectFirst(unlift { case (items, sc) =>
          items.collectFirst(unlift { item =>
            val LR0StateItem(lc, rc, ric, _) = item
            if (left == lc && (right sameElements rc) && (rightIndex + 1) == ric && gotoTable.contains(state -> sc)) {
              Some(item)
            } else {
              None
            }
          }).map { (_, sc) }
        }).foreach { case (item, sc) =>
          if (la == SharpClass) {
            propagation.getOrElseUpdate((state, kItem), mutable.Set.empty) += Tuple2(sc, item)
          } else {
            result.getOrElseUpdate(sc, mutable.Map.empty).getOrElseUpdate(item, mutable.Set.empty) += la
            stack.push(((sc, item), mutable.Set(la)))
          }
        }
      }

      while (stack.nonEmpty) {
        val (ias, laS) = stack.pop()
        propagation.getOrElseUpdate(ias, mutable.Set.empty).foreach { propTo =>
          val (state, item) = propTo
          val set = result.getOrElseUpdate(state, mutable.Map.empty).getOrElseUpdate(item, mutable.Set.empty)
          if (!laS.subsetOf(set)) {
            set ++= laS
            stack.push((propTo, set))
          }
        }
      }

      mutable.Map.newBuilder.++=(
        result.map { case (state, itemAndLa) =>
          val kernel = itemAndLa.flatMap { case (item, laS) => laS.map { LR1StateItem(item, _) } }.toSeq
          val res = lr1closure(mutable.Set(kernel:_*)) -> state
          res
        }
      ).result()
    }

    private def lr0closure(terms: LR0ItemSet): LR0ItemSet = {
      val stack = mutable.Stack[LR0StateItem]()
      stack.pushAll(terms)

      while (stack.nonEmpty) {
        val LR0StateItem(_, right, currentIndex, _) = stack.pop()
        if (currentIndex < right.length) {
          right(currentIndex) match {
            case Term.NonTerminal(tmp) => {
              grammar.getOrElse(tmp, Nil).foreach { case (tmpRight, generator) =>
                val candidate = LR0StateItem(tmp, tmpRight, 0, generator)
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

    private def lr0goto(terms: LR0ItemSet, symbol: Term): LR0ItemSet = {
      val tmp: LR0ItemSet = mutable.Set()
      for (LR0StateItem(left, right, currentIndex, generator) <- terms if currentIndex < right.length && right(currentIndex) == symbol) {
        tmp += LR0StateItem(left, right, currentIndex + 1, generator)
      }
      lr0closure(tmp)
    }

    private def lr1closure(terms: LR1ItemSet): LR1ItemSet = {
      val stack = mutable.Stack[LR1StateItem]()
      stack.pushAll(terms)

      while (stack.nonEmpty) {
        val LR1StateItem(LR0StateItem(_, right, currentIndex, _), la) = stack.pop()
        if (currentIndex < right.length) {
          right(currentIndex) match {
            case Term.NonTerminal(tmp) => {
              grammar.getOrElse(tmp, Nil).foreach { case (tmpRight, generator) =>
                val follows = right.slice(currentIndex + 1, right.length).toSeq

                for (c <- firstOf(follows ++ Seq(Terminal(la)))) {
                  val candidate = LR1StateItem(LR0StateItem(tmp, tmpRight, 0, generator), c)
                  if (!terms.contains(candidate)) {
                    stack.push(candidate)
                    terms += candidate
                  }
                }
              }
            }
            case Term.Terminal(_) =>
          }
        }
      }

      terms
    }

    private def lr1goto(terms: LR1ItemSet, symbol: Term): LR1ItemSet = {
      val tmp: LR1ItemSet = mutable.Set()
      for (LR1StateItem(LR0StateItem(left, right, currentIndex, generator), la) <- terms
            if currentIndex < right.length && right(currentIndex) == symbol) {
        tmp += LR1StateItem(LR0StateItem(left, right, currentIndex + 1, generator), la)
      }
      lr1closure(tmp)
    }

    private def lastReduceRule(arr: Seq[ParseResult[Value]]): Value = {
      arr.head match {
        case ParseResult.Value(value) => value
        case _ => throw new IllegalStateException()
      }
    }

    private def firstOf(seq: Seq[Term]): Set[TokenType] = {
      val result = Set.newBuilder[TokenType]
      var i = 0
      while (i < seq.length) {
        val c = seq(i)
        result ++= first(c)
        i = if (nullable.contains(c)) i + 1 else seq.length
      }
      result.result()
    }
  }

  object ParserBuilder {
    case class LR0StateItem[Value](left: NonTerminalSymbol, right: Array[Term], dot: Int, generator: ResultGenerator[Value]) {
      override def hashCode(): Int = (31 * 31) * left.hashCode() + 31 * util.Arrays.hashCode(right.asInstanceOf[Array[Object]]) + dot

      override def equals(obj: Any): Boolean = {
        if (!obj.isInstanceOf[LR0StateItem[Value]]) return false
        val that = obj.asInstanceOf[LR0StateItem[Value]]

        this.left == that.left && (this.right sameElements that.right) && this.dot == that.dot
      }
    }
    case class LR1StateItem[Value](item: LR0StateItem[Value], lookahead: TokenType)
  }

  sealed abstract class ParseResult[+Value] {
    def asToken: common.Token
    def asValue: Value
  }

  object ParseResult {
    final case class Token[+Value] private[parser](token: common.Token) extends ParseResult[Value] {
      override def asToken: common.Token = token
      override def asValue: Value = throw new NotImplementedError("ParseResult.Value was expected, but it is ParseResult.Token")
    }
    final case class Value[+Value] private[parser](value: Value) extends ParseResult[Value] {
      override def asToken: common.Token = throw new NotImplementedError("ParseResult.Token was expected, but it is ParseResult.Value")
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
