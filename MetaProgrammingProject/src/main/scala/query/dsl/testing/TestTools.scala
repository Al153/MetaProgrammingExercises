package query

import scala.language.higherKinds

/**
  * Methods to test equality of queries and their results.
  */
trait TestTools[M[_], Se[_], Pair[_, _], Single[_], Valid[_]] {

  /**
    * Test the equality of un-executed pair queries
    */
  def equalQuery[A: Valid, B: Valid](p: Pair[A, B], q: Pair[A, B]): M[Boolean]

  /**
    * Test the equality of un-executed single queries
    */
  def equalQuery[A: Valid](s: Single[A], q: Single[A]): M[Boolean]


  /**
    * Test the equality of executed single queries.
    */
  def equalResult[A: Valid](s: Se[A], t: Se[A]): M[Boolean]


  /**
    * Test the equality of executed pair queries
    */
  def equalResult[A: Valid, B: Valid](s: Se[(A, B)], t: Se[(A, B)]): M[Boolean]
}