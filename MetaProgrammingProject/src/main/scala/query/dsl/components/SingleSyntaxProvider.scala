package query.dsl.components

trait SingleSyntaxProvider[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]] {
  self: WithSingleQueries[Pair, Single, Find, Valid] with WithPairQueries[Pair, Single, Valid] =>

  implicit class SingleSyntax[A: Valid](s: Single[A]) {
    private val sq = singleQueries

    import sq._

    def >>[B: Valid](p: Pair[A, B]): Single[B] = from[A, B](s, p)

    def &(t: Single[A]): Single[A] = sq.and(s, t)

    def |(t: Single[A]): Single[A] = sq.or(s, t)
  }

}
