package query.dsl.components

trait Backend[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]] {
  self: Monad[M] =>

  def readPair[A: Valid, B: Valid](p: Pair[A, B]): M[Se[(A, B)]]

  def readSingle[A: Valid](s: Single[A]): M[Se[A]]

  def shortestPath[A: Valid](start: A, end: A, p: Pair[A, A]): M[Option[Path[A]]]

  def allShortestPaths[A: Valid](start: A, p: Pair[A, A]): M[Se[Path[A]]]

  def insert[A: Valid, B: Valid](relations: Seq[R[A, B]]): M[Unit]
}
