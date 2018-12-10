import query.dsl.{Backend, Monad, PairQueries}

import scala.collection.mutable

package object trivial {
  type Id[A] = A
  type Relation[A, B] = Set[(A, B)]

  /**
    * sealed trait Relation[A, B] {
    * def fold[C](c: => C, f: Set[(A,B)] => C): C
    * def reverse: Relation[B, A]
    * def and(relation: Relation[A, B]): Relation[A, B]
    * def or(relation: Relation[A, B]): Relation[A, B]
    * def join[C](relation: Relation[B, C]): Relation[A, C]
    * def andRight(s: Set[B]): Relation[A, B]
    * def andLeft(s: Set[A]): Relation[A, B]
    * }
    * case class S[A, B](s: Set[(A, B)]) extends Relation[A, B] {
    * override def fold[C](c: => C, f: Set[(A, B)] => C): C = f(s)
    * *
    * override def reverse: Relation[B, A] = S(s.map{case (a, b) => (b, a)})
    * *
    * override def and(relation: Relation[A, B]): Relation[A, B] = ???
    * *
    * override def or(relation: Relation[A, B]): Relation[A, B] = ???
    * *
    * override def join[C](relation: Relation[B, C]): Relation[A, C] = ???
    * *
    * override def andRight(s: Set[B]): Relation[A, B] = ???
    * *
    * override def andLeft(s: Set[A]): Relation[A, B] = ???
    * }
    * case class I[A]() extends Relation[A, A] {
    * override def fold[C](c: => C, f: Set[(A, A)] => C): C = c
    * *
    * override def reverse: Relation[A, A] = I()
    * *
    * override def and(relation: Relation[A, A]): Relation[A, A] = relation match {
    * case I()
    * }
    * *
    * override def or(relation: Relation[A, A]): Relation[A, A] = ???
    * *
    * override def join[C](relation: Relation[A, C]): Relation[A, C] = ???
    * *
    * override def andRight(s: Set[A]): Relation[A, A] = ???
    * *
    * override def andLeft(s: Set[A]): Relation[A, A] = ???
    * }
    */

  trait IdMonad extends Monad[Id] {
    override def bind[A, B](ma: Id[A], f: A => Id[B]): Id[B] = f(ma)

    override def point[A](a: => A): Id[A] = a
  }


  case class Universe[A](u: Set[A])

  object TrivialBackend
    extends Backend[Id, Set, Relation, Set, Set, Vector, Relation, Universe]
      with IdMonad {

    class TrivialPairs extends PairQueries[Relation, Set, Universe] {
      override def reverse[A: Universe, B: Universe](p: Relation[A, B]): Relation[B, A] = p map {case (a, b) => b -> a}

      override def and[A: Universe, B: Universe](p: Relation[A, B], q: Relation[A, B]): Relation[A, B] = p intersect q

      override def or[A: Universe, B: Universe](p: Relation[A, B], q: Relation[A, B]): Relation[A, B] = p union q

      override def andRight[A: Universe, B: Universe](p: Relation[A, B], s: Set[B]): Relation[A, B] = p filter { case (_, b) => s contains b }

      override def andLeft[A: Universe, B: Universe](p: Relation[A, B], s: Set[A]): Relation[A, B] = p filter { case (a, _) => s contains a }

      override def chain[A: Universe, B: Universe, C: Universe](p: Relation[A, B], q: Relation[B, C]): Relation[A, C] = joinSet(p, q)

      override def id[A: Universe]: Relation[A, A] = implicitly[Universe[A]].u.map(a => a -> a)

      override def distinct[A: Universe, B: Universe](p: Relation[A, B]): Relation[A, B] = p filter { case (a, b) => a != b }

      override def exactly[A: Universe](p: Relation[A, A], n: Int): Relation[A, A] = if (n <= 0) id else chain(p, exactly(p, n - 1))

      override def upto[A: Universe](p: Relation[A, A], n: Int): Relation[A, A] = exactly(or(p, id), n)

      override def fixedPoint[A: Universe](p: Relation[A, A]): Relation[A, A] = {
        def aux(r: Relation[A, A]): Relation[A, A] = {
          val r2 = chain(r, r)
          if (r2 == r) r else aux(r2)
        }

        aux(upto(p, 2))
      }
    }

    override def readPair[A: Universe, B: Universe](p: Relation[A, B]): Id[Set[(A, B)]] = p

    override def readSingle[A: Universe](s: Set[A]): Id[Set[A]] = s

    override def shortestPath[A: Universe](start: A, end: A, p: Relation[A, A]): Id[Vector[A]] = ???

    override def allShortestPaths[A: Universe](start: A, p: Relation[A, A]): Id[Set[Vector[A]]] = ???

    override def insert[A: Universe, B: Universe](relations: Seq[Relation[A, B]]): Id[Unit] = () // constant value for now ...
  }


  def joinSet[A, B, C](leftRes: Relation[A, B], rightRes: Relation[B, C]): Relation[A, C] = {
    // build an index of all values to join on right, since Proj_B(right) is a subset of Proj_B(Left)
    val collectedRight = mutable.Map[B, mutable.Set[C]]()
    for ((b, c) <- rightRes) {
      val s = collectedRight.getOrElseUpdate(b, mutable.Set())
      s += c
    }

    for {
      (left, middle) <- leftRes
      right <- collectedRight.getOrElse(middle, Set[C]())
    } yield (left, right)
  }

  case object MissingIdRelation extends Throwable

}
