package query.dsl.components

trait PairQueries[Pair[_, _], Single[_], Valid[_]]
  extends SimplePairs[Pair, Single, Valid]
    with SimpleRepetition[Pair, Valid]
    with FixedPoint[Pair, Valid]

trait SimplePairs[Pair[_, _], Single[_], Valid[_]] {
  def reverse[A: Valid, B: Valid](p: Pair[A, B]): Pair[B, A]

  def and[A: Valid, B: Valid](p: Pair[A, B], q: Pair[A, B]): Pair[A, B]

  def or[A: Valid, B: Valid](p: Pair[A, B], q: Pair[A, B]): Pair[A, B]

  def andRight[A: Valid, B: Valid](p: Pair[A, B], s: Single[B]): Pair[A, B]

  def andLeft[A: Valid, B: Valid](p: Pair[A, B], s: Single[A]): Pair[A, B]

  def chain[A: Valid, B: Valid, C: Valid](p: Pair[A, B], q: Pair[B, C]): Pair[A, C]

  def id[A: Valid]: Pair[A, A]

  def distinct[A: Valid, B: Valid](p: Pair[A, B]): Pair[A, B]
}

trait SimpleRepetition[Pair[_, _], Valid[_]] {
  def exactly[A: Valid](p: Pair[A, A], n: Int): Pair[A, A]

  def upto[A: Valid](p: Pair[A, A], n: Int): Pair[A, A]
}

trait FixedPoint[Pair[_, _], Valid[_]] {
  def fixedPoint[A: Valid](p: Pair[A, A]): Pair[A, A]
}