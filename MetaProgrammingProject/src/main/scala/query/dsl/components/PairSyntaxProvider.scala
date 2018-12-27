package query.dsl.components

import scala.language.higherKinds

/**
  * Trait providing pair syntax.
  *
  * A lot of this file appears red in Intellij, but it does compile!
  *
  * @tparam Pair   - pair query type
  * @tparam Single - single query type
  * @tparam Valid
  */
trait PairSyntaxProvider[Pair[_, _], Single[_], Valid[_]] {
  self: SimplePairs[Pair, Single, Valid] =>

  /**
    * The syntax implicit class
    */
  implicit class PairSyntax[A: Valid, B: Valid](p: Pair[A, B]) {


    def --><--[C: Valid](q: Pair[C, B]): Pair[A, C] =
      if (p == q) {
        chain(p, reverse(q)).distinct
      } else chain(p, reverse(q))


    def ->>-(s: Single[B]): Pair[A, B] = andRight(p, s)

    def <---->[C: Valid](q: Pair[A, C]): Pair[B, C] =
      if (p == q) {
        chain(reverse(p), q).distinct
      } else chain(reverse(p), q)

    def -->-->[C: Valid](q: Pair[B, C]): Pair[A, C] = chain(p, q)

    def rev: Pair[B, A] = reverse(p)

    def distinct: Pair[A, B] = self.distinct(p)

    def &(q: Pair[A, B]): Pair[A, B] = and(p, q)

    def |(q: Pair[A, B]): Pair[A, B] = or(p, q)

    def -->(s: Single[B]): HalfPairSyntax[A, B] = HalfPairSyntax[A, B](p, s)

    def <--(s: Single[A]): HalfPairSyntax[B, A] = HalfPairSyntax[B, A](reverse(p), s)
  }

  /**
    * Additional hidden object to allow more complex constructions, like
    *
    * p --> m --> q
    */
  case class HalfPairSyntax[A: Valid, B: Valid] private(p: Pair[A, B], m: Single[B]) {

    def -->[C: Valid](q: Pair[B, C]): Pair[A, C] = distinct(chain(andRight(p, m), q))

    def <--[C: Valid](q: Pair[C, B]): Pair[A, C] = distinct(chain(andRight(p, m), reverse(q)))
  }

}
