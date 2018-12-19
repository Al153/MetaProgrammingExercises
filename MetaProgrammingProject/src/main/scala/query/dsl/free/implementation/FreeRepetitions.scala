package query.dsl.free.implementation

import query.dsl.components.{SimplePairs, SimpleRepetition}

import scala.language.higherKinds

/**
  * Freely implement repetition
  * @tparam Pair  pair query type
  * @tparam Single
  * @tparam Valid validity typeclass
  */
trait FreeRepetitions[
Pair[_, _], Single[_], Valid[_]
] extends SimpleRepetition[Pair, Valid] {
  pairs: SimplePairs[Pair, Single, Valid] =>

  final override def exactly[A: Valid](p: Pair[A, A], n: Int): Pair[A, A] =
    if (n < 0) exactly(pairs.reverse(p), -n) else if (n == 0) pairs.id[A]
    else if (n == 0) p else if (n % 2 == 0) exactly(pairs.chain(p, p), n / 2)
    else pairs.chain(p, exactly(pairs.chain(p, p), (n - 1) / 2))

  final override def upto[A: Valid](p: Pair[A, A], n: Int): Pair[A, A] = exactly(pairs.or(p, pairs.id), n)
}