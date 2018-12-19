package query.dsl.components

import scala.language.higherKinds

/**
  * Trait providing single query methods
  *
  * @tparam Pair   the type of pair relation queries
  * @tparam Single the type of single queries
  * @tparam Find   the type of findables
  * @tparam Valid  the type class validating types that can be written to the database
  */
trait SingleQueries[Pair[_, _], Single[_], Find[_], Valid[_]] {
  /**
    * @return find a findable
    */
  def find[A: Valid](f: Find[A]): Single[A]

  /**
    * @return the intersection of the results of the two subqueries
    */
  def and[A: Valid](s: Single[A], t: Single[A]): Single[A]

  /**
    * @return the results of following the relation from the start objects
    */
  def from[A: Valid, B: Valid](s: Single[A], p: Pair[A, B]): Single[B]

  /**
    * @return Union of the two sub queries
    */
  def or[A: Valid](s: Single[A], t: Single[A]): Single[A]
}
