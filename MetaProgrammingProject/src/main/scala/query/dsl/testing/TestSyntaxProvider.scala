package query.dsl.testing

import query.TestTools

import scala.language.higherKinds

/**
  * Provides syntax for equality testing.
  *
  * @tparam M      - Monad return type
  * @tparam Se     - return set type
  * @tparam Pair   - pair query type
  * @tparam Single - single query type
  * @tparam Valid  - Validity type-class
  */
trait TestSyntaxProvider[M[_], Se[_], Pair[_, _], Single[_], Valid[_]] {
  self: TestTools[M, Se, Pair, Single, Valid] =>

  implicit class PairTestSyntax[A: Valid, B: Valid](p: Pair[A, B]) {
    def =~=(q: Pair[A, B]): M[Boolean] = equalQuery(p, q)
  }

  implicit class SingleTestSyntax[A: Valid](s: Single[A]) {
    def =~=(t: Single[A]): M[Boolean] = equalQuery(s, t)
  }

  implicit class ResultSetSingleSyntax[A: Valid, B: Valid](r: Se[(A, B)]) {
    def ===(s: Se[(A, B)]): M[Boolean] = equalResult(r, s)
  }

  implicit class ResultSetPairSyntax[A: Valid](r: Se[A]) {
    def ===(s: Se[A]): M[Boolean] = equalResult(r, s)
  }

}