package jp.pois.pg4scala
package utils

import scala.collection.Searching
import scala.collection.Searching.search

private[pg4scala] class IntArrayIndicatorMap[+V](private val domain: Array[Int], private val value: V) extends Map[Int, V] {
  override def +[V1 >: V](kv: (Int, V1)): Map[Int, V1] = {
    val builder = Map.newBuilder[Int, V1]
    for (e <- domain) builder += Tuple2(e, value)
    builder += kv
    builder.result()
  }

  override def get(key: Int): Option[V] = domain.search(key) match {
    case Searching.Found(_) => Some(value)
    case Searching.InsertionPoint(_) => None
  }

  override def iterator: Iterator[(Int, V)] = domain.iterator.map { (_, value) }

  override def -(key: Int): Map[Int, V] = domain.search(key) match {
    case Searching.Found(foundIndex) => {
      val newArray = new Array[Int](domain.length - 1)
      Array.copy(domain, 0, newArray, 0, foundIndex)
      Array.copy(domain, foundIndex + 1, newArray, foundIndex, domain.length - 1 - foundIndex)
      new IntArrayIndicatorMap(newArray, value)
    }
    case Searching.InsertionPoint(_) => this
  }
}
