package query.dsl.components

trait PairSyntaxProvider[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]] {
  self: WithPairQueries[Pair, Single, Valid] =>

  implicit class PairSyntax[A: Valid, B: Valid](p: Pair[A, B]) {
    private val pq = pairQueries

    import pq._

    def --><--[C: Valid](q: Pair[C, B]): Pair[A, C] = chain(p, reverse(q))

    def ->>-(s: Single[B]): Pair[A, B] = andRight(p, s)

    // moves away from the original DSL
    def ->>-:(s: Single[A]): Pair[A, B] = andLeft(p, s)

    def <---->[C: Valid](q: Pair[A, C]): Pair[B, C] = chain(reverse(p), q)

    def -->-->[C: Valid](q: Pair[B, C]): Pair[A, C] = chain(p, q)

    def rev: Pair[B, A] = reverse(p)

    def distinct: Pair[A, B] = pq.distinct(p)

    def &(q: Pair[A, B]): Pair[A, B] = and(p, q)

    def |(q: Pair[A, B]): Pair[A, B] = or(p, q)

    def -->(s: Single[B]): HalfPairSyntax[A, B] = HalfPairSyntax[A, B](p, s)

    def <--(s: Single[A]): HalfPairSyntax[B, A] = HalfPairSyntax[B, A](reverse(p), s)
  }

  case class HalfPairSyntax[A: Valid, B: Valid] private(p: Pair[A, B], m: Single[B]) {
    val pq = pairQueries

    import pq._

    def -->[C: Valid](q: Pair[B, C]): Pair[A, C] = distinct(chain(andRight(p, m), q))

    def <--[C: Valid](q: Pair[C, B]): Pair[A, C] = distinct(chain(andRight(p, m), reverse(q)))
  }

}
