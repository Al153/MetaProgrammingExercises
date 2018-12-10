package query.dsl.free.implementation

import query.dsl.components.{SimplePairs, SimpleRepetition}

class FreeRepetitions[
Pair[_, _], Single[_], Valid[_]
](
   pairs: SimplePairs[Pair, Single, Valid]
 ) extends SimpleRepetition[Pair, Valid] {
  override def exactly[A: Valid](p: Pair[A, A], n: Int): Pair[A, A] =
    if (n < 0) exactly(pairs.reverse(p), -n) else if (n == 0) pairs.id[A]
    else if (n == 0) p else if (n % 2 == 0) exactly(pairs.chain(p, p), n / 2)
    else pairs.chain(p, exactly(pairs.chain(p, p), (n - 1) / 2))

  override def upto[A: Valid](p: Pair[A, A], n: Int): Pair[A, A] = exactly(pairs.or(p, pairs.id), n)
}