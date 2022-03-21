package jp.pois.pg4scala
package lexer.util

import org.scalatest.funsuite.AnyFunSuite

class FilterNotIterableTest extends AnyFunSuite{
  test("correctElements") {
    val iter = new FilterNotIterable((0 until 100), Range(0, 100, 2)).toArray
    assert(iter sameElements Range(1, 100, 2))
  }

}
