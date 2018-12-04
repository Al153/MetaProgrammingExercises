package query.dsl.components

trait SymmetricSyntaxProvider[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]] {
  self: WithPairQueries[Pair, Single, Valid] =>

  implicit class SymmetricSyntax[A: Valid](p: Pair[A, A]) {
    private val pq = pairQueries

    import pq._

    def *(n: Int): Pair[A, A] = doExactly(n)

    def *(r: Repetition): Pair[A, A] = r match {
      case BetweenRange(lo, hi) => chain(doExactly(lo), createUpto(hi - lo))
      case UptoRange(n) => createUpto(n)
      case AtleastRange(n) =>
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
