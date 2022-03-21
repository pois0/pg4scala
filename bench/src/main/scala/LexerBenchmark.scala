package jp.pois.pg4scala

import lexer.Lexer

import org.openjdk.jmh.annotations._

import java.io.StringReader
import scala.io.Source

@Warmup(iterations = 2)
@Fork(1)
@Measurement(iterations = 5)
@State(Scope.Benchmark)
class LexerBenchmark {
  private var lexer: Lexer = _
  private var jsonString: String = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    lexer = JsonToken.jsonLexerBuilder.build

    val source = Source.fromResource("large-file.json")
    jsonString = try source.mkString finally source.close()
  }

  @Benchmark
  def lexJson(): Unit = {
    lexer.lex(new StringReader(jsonString)).size
  }
}
