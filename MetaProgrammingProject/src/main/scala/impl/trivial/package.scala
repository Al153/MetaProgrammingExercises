package impl

import impl.common.Id
import query.dsl.DSL
import query.dsl.components._
import query.dsl.free.implementation.FreeRepetitions

import scala.collection.immutable.Queue
import scala.collection.mutable
import common._

/**
  * Very naive.
  * A slightly ugly, imperative, non-monadic set based relation implementation
  * Uses a few optimisations, but only to simplify writing it
  */
package object trivial {

  type Relation[A, B] = Set[(A, B)]



  case class Universe[A](u: Set[A])

  object TrivialBackend
    extends DSL[Id, Set, Relation, Set, Set, Vector, Relation, Universe]
      with HasMonad[Id] with FreeRepetitions[Relation, Set, Universe] {


    override def readPair[A: Universe, B: Universe](p: Relation[A, B]): Id[Set[(A, B)]] = {
      val AU = implicitly[Universe[A]].u
      val BU = implicitly[Universe[B]].u
      // filter for A and B universe so that identity sets work.
      p filter { case (a, b) => AU.contains(a) && BU.contains(b) }
    }

    override def readSingle[A: Universe](s: Set[A]): Id[Set[A]] = s

    override def shortestPath[A: Universe](start: A, end: A, p: Relation[A, A]): Id[Option[Vector[A]]] = {

      // create a mutable index for traversal.
      val index: mutable.HashMap[A, Set[A]] = new mutable.HashMap[A, Set[A]]()

      def searchStep(a: A): Set[A] = index.getOrElse(a, Set())

      p.foreach {
        case (from, to) =>
          if (index.contains(from))
            index(from) = index(from) + to
          else
            index(from) = Set(to)
      }

      var fringe: Queue[List[A]] = Queue(List(start))
      var alreadyExplored: Set[A] = Set()
      var result: Option[List[A]] = None
      var done = false

      while (fringe.nonEmpty && !done) {

        val stepResult = doStep(searchStep, fringe, alreadyExplored)
        val (newFringe, path, objects) = stepResult
        if (objects.contains(end)) {
          done = true
          result = Some(end :: path)
        } else {
          fringe = newFringe
          alreadyExplored = alreadyExplored | objects
          result = None
        }

      }
      result.map(_.reverse.toVector)
    }

    override def allShortestPaths[A: Universe](start: A, p: Relation[A, A]): Id[Set[Vector[A]]] = {
      // create a mutable index for traversal.
      val index: mutable.HashMap[A, Set[A]] = new mutable.HashMap[A, Set[A]]()

      def searchStep(a: A): Set[A] = index.getOrElse(a, Set())

      p.foreach {
        case (from, to) =>
          if (index.contains(from))
            index(from) = index(from) + to
          else
            index(from) = Set(to)
      }


      var fringe: Queue[List[A]] = Queue(List(start))
      var alreadyExplored: Set[A] = Set()
      var resBuilder: mutable.Builder[List[A], Set[List[A]]] = Set.newBuilder[List[A]]


      var generationCount = 0
      var nodeArity: Long = 0
      while (fringe.nonEmpty) {
        generationCount += 1
        val stepResult = doStep(searchStep, fringe, alreadyExplored)
        val (newFringe, path, objects) = stepResult
        nodeArity += objects.size
        resBuilder ++= objects.map(_ :: path)
        fringe = newFringe
        alreadyExplored = alreadyExplored | objects
      }


      resBuilder.result().map(_.reverse.toVector)
    }

    private def doStep[E, A](searchStep: A => Set[A], fringe: Queue[List[A]], alreadyExplored: Set[A]): (Queue[List[A]], List[A], Set[A]) =
      if (fringe.nonEmpty) {
        val top = fringe.head // pop the top off of the fringe
        val next = searchStep(top.head)
        val newObjects = next.diff(alreadyExplored)
        val newFringe = fringe.tail ++ newObjects.diff(alreadyExplored).map(_ :: top)
        (newFringe, top, newObjects)
      } else {
        (fringe, List(), alreadyExplored)
      }

    override def insert[A: Universe, B: Universe](relations: Seq[Relation[A, B]]): Id[Unit] = () // constant value for now ...

    override def reverse[A: Universe, B: Universe](p: Relation[A, B]): Relation[B, A] = p map { case (a, b) => b -> a }

    override def and[A: Universe, B: Universe](p: Relation[A, B], q: Relation[A, B]): Relation[A, B] = p intersect q

    override def or[A: Universe, B: Universe](p: Relation[A, B], q: Relation[A, B]): Relation[A, B] = p union q

    override def andRight[A: Universe, B: Universe](p: Relation[A, B], s: Set[B]): Relation[A, B] = p filter { case (_, b) => s contains b }

    override def andLeft[A: Universe, B: Universe](p: Relation[A, B], s: Set[A]): Relation[A, B] = p filter { case (a, _) => s contains a }

    override def id[A: Universe]: Relation[A, A] = implicitly[Universe[A]].u.map(a => a -> a)

    override def distinct[A: Universe, B: Universe](p: Relation[A, B]): Relation[A, B] = p filter { case (a, b) => a != b }

    override def fixedPoint[A: Universe](p: Relation[A, A]): Relation[A, A] = {
      def aux(r: Relation[A, A]): Relation[A, A] = {
        val r2 = chain(r, r)
        if (r2 == r) r else aux(r2)
      }

      aux(upto(p, 2))
    }

    override def chain[A: Universe, B: Universe, C: Universe](p: Relation[A, B], q: Relation[B, C]): Relation[A, C] = joinSet(p, q)

    override def find[A: Universe](f: Set[A]): Set[A] = f & implicitly[Universe[A]].u

    override def and[A: Universe](s: Set[A], t: Set[A]): Set[A] = s & t

    override def from[A: Universe, B: Universe](s: Set[A], p: Relation[A, B]): Set[B] =
      p collect { case (a, b) if s.contains(a) => b }

    override def or[A: Universe](s: Set[A], t: Set[A]): Set[A] = s | t

    override implicit def m: Monad[Id] = IdMonad


  }
}
