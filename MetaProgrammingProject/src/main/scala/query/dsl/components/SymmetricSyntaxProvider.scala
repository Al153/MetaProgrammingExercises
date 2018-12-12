package query.dsl.components

trait SymmetricSyntaxProvider[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], ToInsert[_, _], Valid[_]] {
  self: WithSimplePairs[Pair, Single, Valid]
    with WithSimpleRepetition[Pair, Valid]
    with WithFixedPoint[Pair, Valid] =>

  implicit class SymmetricSyntax[A: Valid](p: Pair[A, A]) {
    private val rep = simpleRepetition
    private val sp = simplePairs
    private val fp = fixedPoint


    import fp._
    import rep._
    import sp._

    def *(n: Int): Pair[A, A] = doExactly(n)

    def *(r: Repetition): Pair[A, A] = r match {
      case BetweenRange(lo, hi) => chain(doExactly(lo), createUpto(hi - lo))
      case UptoRange(n) => createUpto(n)
      case AtleastRange(n) =>
        if (n == 0) fp.fixedPoint(p)
        else chain(doExactly(n), fp.fixedPoint(p))
    }

    private def doExactly(n: Int): Pair[A, A] =
      if (n >= 0) exactly(p, n) else exactly(reverse(p), -n)

    private def createUpto(n: Int): Pair[A, A] =
      if (n >= 0) upto(p, n) else upto(reverse(p), -n)

    def ** : Pair[A, A] = fp.fixedPoint(p)

    def ++ : Pair[A, A] = chain(p, fp.fixedPoint(p))

    def ? : Pair[A, A] = upto(p, 1)
  }

}
