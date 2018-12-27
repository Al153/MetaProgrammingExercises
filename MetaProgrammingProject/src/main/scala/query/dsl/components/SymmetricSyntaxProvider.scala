package query.dsl.components

import scala.language.higherKinds

/**
  * Trait providing syntax for symmetric (repetition based) queries.
  *
  * @tparam Pair   The type of pair based queries
  * @tparam Single The type of single queries
  * @tparam Valid  The typeclass ensuring validity of types put in the database.
  */
trait SymmetricSyntaxProvider[Pair[_, _], Single[_], Valid[_]] {
  // Requires these dependencies
  self: SimplePairs[Pair, Single, Valid]
    with SimpleRepetition[Pair, Valid]
    with FixedPoint[Pair, Valid] =>

  implicit class SymmetricSyntax[A: Valid](p: Pair[A, A]) {

    def *(n: Int): Pair[A, A] = doExactly(n)

    def *(r: Repetition): Pair[A, A] = r match {
      case Between(lo, hi) => chain(doExactly(lo), createUpto(hi - lo))
      case Upto(n) => createUpto(n)
      case AtLeast(n) =>
        if (n == 0) fixedPoint(p)
        else chain(doExactly(n), fixedPoint(p))
    }

    private def doExactly(n: Int): Pair[A, A] =
      if (n >= 0) exactly(p, n) else exactly(reverse(p), -n)

    private def createUpto(n: Int): Pair[A, A] =
      if (n >= 0) upto(p, n) else upto(reverse(p), -n)

    def ** : Pair[A, A] = fixedPoint(p)

    def ++ : Pair[A, A] = chain(p, fixedPoint(p))

    def ? : Pair[A, A] = upto(p, 1)
  }

}
