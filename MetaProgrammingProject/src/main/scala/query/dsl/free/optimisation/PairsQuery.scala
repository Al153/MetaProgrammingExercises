package query.dsl.free.optimisation

import query.dsl.DSL
import query.dsl.components.Monad

import scala.language.{higherKinds, implicitConversions}


case class OptimisingDSL[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], ToInsert[_, _], Valid[_]](
                                                                                                          dsl: DSL[M, Se, Pair, Single, Find, Path, ToInsert, Valid]
                                                                                                        ) {
  // sealed trait OptimisedPairs[]
  sealed abstract class PairsQuery[A: Valid, B: Valid] {
    def render: Pair[A, B]
  }

  case class Pr[A: Valid, B: Valid](p: Pair[A, B]) extends PairsQuery[A, B] {
    override def render: Pair[A, B] = p
  }

  case class And[A: Valid, B: Valid](p: PairsQuery[A, B], q: PairsQuery[A, B]) extends PairsQuery[A, B] {
    override def render: Pair[A, B] = dsl.and(p.render, q.render)
  }

  case class Or[A: Valid, B: Valid](p: PairsQuery[A, B], q: PairsQuery[A, B]) extends PairsQuery[A, B] {
    override def render: Pair[A, B] = dsl.or(p.render, q.render)
  }

  case class AndRight[A: Valid, B: Valid](p: PairsQuery[A, B], s: SinglesQuery[B]) extends PairsQuery[A, B] {
    override def render: Pair[A, B] = dsl.andRight(p.render, s.render)
  }

  case class AndLeft[A: Valid, B: Valid](p: PairsQuery[A, B], s: SinglesQuery[A]) extends PairsQuery[A, B] {
    override def render: Pair[A, B] = dsl.andLeft(p.render, s.render)
  }

  case class Reverse[A: Valid, B: Valid](p: PairsQuery[A, B]) extends PairsQuery[B, A] {
    override def render: Pair[B, A] = dsl.reverse(p.render)
  }

  case class Distinct[A: Valid, B: Valid](p: PairsQuery[A, B]) extends PairsQuery[A, B] {
    override def render: Pair[A, B] = dsl.distinct(p.render)
  }

  case class Id[A: Valid]() extends PairsQuery[A, A] {
    override def render: Pair[A, A] = dsl.id
  }

  case class Chain[A: Valid, B: Valid, C: Valid](p: PairsQuery[A, B], q: PairsQuery[B, C]) extends PairsQuery[A, C] {
    override def render: Pair[A, C] = dsl.chain(p.render, q.render)
  }

  case class Exactly[A: Valid](p: PairsQuery[A, A], n: Int) extends PairsQuery[A, A] {
    override def render: Pair[A, A] = dsl.exactly(p.render, n)
  }

  case class Upto[A: Valid](p: PairsQuery[A, A], n: Int) extends PairsQuery[A, A] {
    override def render: Pair[A, A] = dsl.upto(p.render, n)
  }

  case class FixedPoint[A: Valid](p: PairsQuery[A, A]) extends PairsQuery[A, A] {
    override def render: Pair[A, A] = dsl.fixedPoint(p.render)
  }

  sealed abstract class SinglesQuery[A: Valid] {
    def render: Single[A]
  }

  case class SFind[A: Valid](f: Find[A]) extends SinglesQuery[A] {
    override def render: Single[A] = dsl.find(f)
  }

  case class OrS[A: Valid](s: SinglesQuery[A], t: SinglesQuery[A]) extends SinglesQuery[A] {
    override def render: Single[A] = dsl.or(s.render, t.render)
  }

  case class AndS[A: Valid](s: SinglesQuery[A], t: SinglesQuery[A]) extends SinglesQuery[A] {
    override def render: Single[A] = dsl.and(s.render, t.render)
  }

  case class From[A: Valid, B: Valid](s: SinglesQuery[A], p: PairsQuery[A, B]) extends SinglesQuery[B] {
    override def render: Single[B] = dsl.from(s.render, p.render)
  }

  implicit def toPQ[A: Valid, B: Valid](p: Pair[A, B]): PairsQuery[A, B] = Pr(p)


  def buildDSL: DSL[M, Se, PairsQuery, SinglesQuery, Find, Path, ToInsert, Valid] =
    new DSL[M, Se, PairsQuery, SinglesQuery, Find, Path, ToInsert, Valid] {
      /**
        * Reverse the direction of the relation
        */
      override def reverse[A: Valid, B: Valid](p: PairsQuery[A, B]): PairsQuery[B, A] = Reverse(p)

      /**
        * @return the intersection of the relations
        */
      override def and[A: Valid, B: Valid](p: PairsQuery[A, B], q: PairsQuery[A, B]): PairsQuery[A, B] = And(p, q)

      /**
        * @return the union of the relations
        */
      override def or[A: Valid, B: Valid](p: PairsQuery[A, B], q: PairsQuery[A, B]): PairsQuery[A, B] = or(p, q)

      /**
        * @return filter the relation to those that match the single query
        */
      override def andRight[A: Valid, B: Valid](p: PairsQuery[A, B], s: SinglesQuery[B]): PairsQuery[A, B] = AndRight(p, s)

      /**
        * @return filter the relation to those pairs where the left value matches the single query
        */
      override def andLeft[A: Valid, B: Valid](p: PairsQuery[A, B], s: SinglesQuery[A]): PairsQuery[A, B] = AndLeft(p, s)

      /**
        * @return inner join two queries
        */
      override def chain[A: Valid, B: Valid, C: Valid](p: PairsQuery[A, B], q: PairsQuery[B, C]): PairsQuery[A, C] = Chain(p, q)

      /**
        * @return the identity relation
        */
      override def id[A: Valid]: PairsQuery[A, A] = Id[A]()

      /**
        * @return related values that are not equal
        */
      override def distinct[A: Valid, B: Valid](p: PairsQuery[A, B]): PairsQuery[A, B] = Distinct(p)

      /**
        *
        * @param p query to repeat
        * @param n times
        * @return chains p to itself n times
        */
      override def exactly[A: Valid](p: PairsQuery[A, A], n: Int): PairsQuery[A, A] = Exactly(p, n)

      /**
        * @param p query to repeats
        * @param n times
        * @return finds pairs related by up to n repetitions of p
        */
      override def upto[A: Valid](p: PairsQuery[A, A], n: Int): PairsQuery[A, A] = Upto(p, n)

      override def shortestPath[A: Valid](start: A, end: A, p: PairsQuery[A, A]): M[Option[Path[A]]] = dsl.shortestPath(start, end, runOptimisations(p).render)

      override def allShortestPaths[A: Valid](start: A, p: PairsQuery[A, A]): M[Se[Path[A]]] = dsl.allShortestPaths(start, runOptimisations(p).render)

      override implicit def m: Monad[M] = dsl.m

      /**
        * @param p base query
        * @return the fixed point of appending p to itself
        */
      override def fixedPoint[A: Valid](p: PairsQuery[A, A]): PairsQuery[A, A] = FixedPoint(p)

      override def readPair[A: Valid, B: Valid](p: PairsQuery[A, B]): M[Se[(A, B)]] = dsl.readPair(runOptimisations(p).render)

      override def readSingle[A: Valid](s: SinglesQuery[A]): M[Se[A]] = dsl.readSingle(runOptimisations(s).render)

      /**
        * @return find a findable
        */
      override def find[A: Valid](f: Find[A]): SinglesQuery[A] = SFind(f)

      /**
        * @return the intersection of the results of the two subqueries
        */
      override def and[A: Valid](s: SinglesQuery[A], t: SinglesQuery[A]): SinglesQuery[A] = AndS(s, t)

      /**
        * @return the results of following the relation from the start objects
        */
      override def from[A: Valid, B: Valid](s: SinglesQuery[A], p: PairsQuery[A, B]): SinglesQuery[B] = From(s, p)

      /**
        * @return Union of the two sub queries
        */
      override def or[A: Valid](s: SinglesQuery[A], t: SinglesQuery[A]): SinglesQuery[A] = OrS(s, t)

      override def insert[A: Valid, B: Valid](relations: Seq[ToInsert[A, B]]): M[Unit] = dsl.insert(relations)


      private def runOptimisations[A: Valid, B: Valid](p: PairsQuery[A, B]): PairsQuery[A, B] = ???

      private def runOptimisations[A: Valid](s: SinglesQuery[A]): SinglesQuery[A] = ???
    }
}

