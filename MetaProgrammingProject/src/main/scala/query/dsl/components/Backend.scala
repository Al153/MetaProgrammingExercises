package query.dsl.components

/**
  * The interpreter trait.
  * An implementation of this trait gives the system the main means to read and write to and from the database
  * @tparam M - Monadic return type
  * @tparam Se - Sequence return type
  * @tparam Pair - type of pair queries
  * @tparam Single - type of single queries
  * @tparam Find - findable filters
  * @tparam Path - type of paths in the database
  * @tparam ToInsert - format in which relations are inserted into the database
  * @tparam Valid - the typeclass specifying which types can be stored
  */
trait Backend[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], ToInsert[_, _], Valid[_]] {
  self: Monad[M] =>

  def readPair[A: Valid, B: Valid](p: Pair[A, B]): M[Se[(A, B)]]

  def readSingle[A: Valid](s: Single[A]): M[Se[A]]

  def shortestPath[A: Valid](start: A, end: A, p: Pair[A, A]): M[Option[Path[A]]]

  def allShortestPaths[A: Valid](start: A, p: Pair[A, A]): M[Se[Path[A]]]

  def insert[A: Valid, B: Valid](relations: Seq[ToInsert[A, B]]): M[Unit]
}
