package query.dsl.components

import scala.language.higherKinds


/**
  * Trait providing syntax for single queries
  *
  * @tparam Pair   The pair query type
  * @tparam Single The single query type
  * @tparam Find   Type of findables
  * @tparam Valid  the typeclass validating items that go in the database.
  */
trait SingleSyntaxProvider[Pair[_, _], Single[_], Find[_], Valid[_]] {
  self: SingleQueries[Pair, Single, Find, Valid] =>

  /**
    * The provided syntax class
    */
  implicit class SingleSyntax[A: Valid](s: Single[A]) {

    def >>[B: Valid](p: Pair[A, B]): Single[B] = from[A, B](s, p)

    def &(t: Single[A]): Single[A] = and(s, t)

    def |(t: Single[A]): Single[A] = or(s, t)
  }

}
