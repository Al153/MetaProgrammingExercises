package query.dsl

trait SingleQueries[Pair[_, _], Single[_], Find[_], Valid[_]] {
  def find[A: Valid](f: Find[A]): Single[A]

  def and[A: Valid](s: Single[A], t: Single[A]): Single[A]

  def from[A: Valid, B: Valid](s: Single[A], p: Pair[A, B]): Single[B]

  def or[A: Valid](s: Single[A], t: Single[A]): Single[A]
}
