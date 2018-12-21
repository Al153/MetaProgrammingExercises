package query.dsl.components

import scala.language.higherKinds

/**
  * The interpreter trait.
  * An implementation of this trait gives the system the main means to read and write to and from the database
  *
  * @tparam M        - Monadic return type
  * @tparam Se       - Sequence return type
  * @tparam Pair     - type of pair queries
  * @tparam Single   - type of single queries
  * @tparam Find     - findable filters
  * @tparam Path     - type of paths in the database
  * @tparam ToInsert - format in which relations are inserted into the database
  * @tparam Valid    - the typeclass specifying which types can be stored
  */
trait Backend[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], ToInsert[_, _], Valid[_]]
  extends Reads[M, Se, Pair, Single, Valid]
    with PathFinding[M, Se, Pair, Path, Valid]
    with Writes[M, ToInsert, Valid]

trait Reads[M[_], Se[_], Pair[_, _], Single[_], Valid[_]] {
  def readPair[A: Valid, B: Valid](p: Pair[A, B]): M[Se[(A, B)]]

  def readSingle[A: Valid](s: Single[A]): M[Se[A]]
}

trait PathFinding[M[_], Se[_], Pair[_, _], Path[_], Valid[_]] {
  def shortestPath[A: Valid](start: A, end: A, p: Pair[A, A]): M[Option[Path[A]]]

  def allShortestPaths[A: Valid](start: A, p: Pair[A, A]): M[Se[Path[A]]]
}

trait Writes[M[_], ToInsert[_, _], Valid[_]] {
  def insert[A: Valid, B: Valid](relations: Seq[ToInsert[A, B]]): M[Unit]
}