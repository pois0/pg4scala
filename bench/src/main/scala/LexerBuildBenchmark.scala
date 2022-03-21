package jp.pois.pg4scala

import lexer.Lexer.LexerBuilder

import org.openjdk.jmh.annotations._


@Warmup(iterations = 2)
@Fork(1)
@Measurement(iterations = 5)
@State(Scope.Benchmark)
class LexerBuildBenchmark {
  var builder: LexerBuilder = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    builder = JsonToken.jsonLexerBuilder
  }

  @Benchmark
  def buildLexer(): Unit = {
    builder.build
  }
}
