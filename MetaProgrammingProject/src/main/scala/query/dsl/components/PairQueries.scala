package query.dsl.components

import scala.language.higherKinds

/**
  * Collected trait of all the types of pair queries
  *
  * @tparam Pair   - pair query type
  * @tparam Single - Single query type
  * @tparam Valid  - type validating type-class
  */
trait PairQueries[Pair[_, _], Single[_], Valid[_]]
  extends SimplePairs[Pair, Single, Valid]
    with SimpleRepetition[Pair, Valid]
    with FixedPoint[Pair, Valid]

/**
  * Simple pair-based relation query combinators
  *
  * @tparam Pair   - pair query type
  * @tparam Single - Single query types
  * @tparam Valid  - validity type class
  */
trait SimplePairs[Pair[_, _], Single[_], Valid[_]] {
  /**
    * Reverse the direction of the relation
    */
  def reverse[A: Valid, B: Valid](p: Pair[A, B]): Pair[B, A]

  /**
    * @return the intersection of the relations
    */
  def and[A: Valid, B: Valid](p: Pair[A, B], q: Pair[A, B]): Pair[A, B]

  /**
    * @return the union of the relations
    */
  def or[A: Valid, B: Valid](p: Pair[A, B], q: Pair[A, B]): Pair[A, B]

  /**
    * @return filter the relation to those that match the single query
    */
  def andRight[A: Valid, B: Valid](p: Pair[A, B], s: Single[B]): Pair[A, B]

  /**
    * @return filter the relation to those pairs where the left value matches the single query
    */
  def andLeft[A: Valid, B: Valid](p: Pair[A, B], s: Single[A]): Pair[A, B]

  /**
    * @return inner join two queries
    */
  def chain[A: Valid, B: Valid, C: Valid](p: Pair[A, B], q: Pair[B, C]): Pair[A, C]

  /**
    * @return the identity relation
    */
  def id[A: Valid]: Pair[A, A]

  /**
    * @return related values that are not equal
    */
  def distinct[A: Valid, B: Valid](p: Pair[A, B]): Pair[A, B]
}

/**
  * Simple repetition combinators
  *
  * @tparam Pair  pair query type
  * @tparam Valid validity typeclass
  */
trait SimpleRepetition[Pair[_, _], Valid[_]] {
  /**
    *
    * @param p query to repeat
    * @param n times
    * @return chains p to itself n times
    */
  def exactly[A: Valid](p: Pair[A, A], n: Int): Pair[A, A]

  /**
    * @param p query to repeats
    * @param n times
    * @return finds pairs related by up to n repetitions of p
    */
  def upto[A: Valid](p: Pair[A, A], n: Int): Pair[A, A]
}

/**
  * Provides the fixed-point combinator
  *
  * @tparam Pair  type of pair relations
  * @tparam Valid validity type-class
  */
trait FixedPoint[Pair[_, _], Valid[_]] {
  /**
    * @param p base query
    * @return the fixed point of appending p to itself
    */
  def fixedPoint[A: Valid](p: Pair[A, A]): Pair[A, A]
}