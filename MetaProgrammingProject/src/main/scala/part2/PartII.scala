package part2

import core.backend.intermediate
import core.backend.intermediate._
import core.user.containers.{Operation, Path}
import core.user.dsl.{CompletedRelation, E, HasRecovery, Relation}
import core.user.interfaces.DBInstance
import core.user.schema.{Findable, SchemaObject}
import query.dsl.DSL
import query.dsl.components.Monad
import query.dsl.testing.RunTimeTestTools

class PartII[Err <: E : HasRecovery] {
  type Op[A] = Operation[Err, A]
  type R[A, B] = (A, Relation[A, B], B)


  def toPartIII(d: DBInstance[Err]):
  DSL[Op, Set, FindPair, FindSingle, Findable, Path, R, SchemaObject]
    with RunTimeTestTools[Op, Set, FindPair, FindSingle, Findable, Path, R, SchemaObject]
  = new DSL[Op, Set, FindPair, FindSingle, Findable, Path, R, SchemaObject]
    with RunTimeTestTools[Op, Set, FindPair, FindSingle, Findable, Path, R, SchemaObject] {

    import d._

    override def fixedPoint[A: SchemaObject](p: FindPair[A, A]): FindPair[A, A] = intermediate.FixedPoint(p)

    override implicit def m: Monad[Op] =
      new Monad[Op] {
        def bind[A, B](ma: Operation[Err, A], f: A => Operation[Err, B]): Operation[Err, B] = ma.flatMap(f)

        override def point[A](a: => A): Operation[Err, A] = Operation.point[Err, A](a)
      }


    override def find[A: SchemaObject](f: Findable[A]): FindSingle[A] = Find(f)

    override def and[A: SchemaObject](s: FindSingle[A], t: FindSingle[A]): FindSingle[A] = AndS(s, t)

    override def from[A: SchemaObject, B: SchemaObject](s: FindSingle[A], p: FindPair[A, B]): FindSingle[B] = From(s, p)

    override def or[A: SchemaObject](s: FindSingle[A], t: FindSingle[A]): FindSingle[A] = OrS(s, t)

    override def reverse[A: SchemaObject, B: SchemaObject](p: FindPair[A, B]): FindPair[B, A] = p.reverse

    override def and[A: SchemaObject, B: SchemaObject](p: FindPair[A, B], q: FindPair[A, B]): FindPair[A, B] = And(p, q)

    override def or[A: SchemaObject, B: SchemaObject](p: FindPair[A, B], q: FindPair[A, B]): FindPair[A, B] = Or(p, q)

    override def andRight[A: SchemaObject, B: SchemaObject](p: FindPair[A, B], s: FindSingle[B]): FindPair[A, B] = AndRight(p, s)

    override def andLeft[A: SchemaObject, B: SchemaObject](p: FindPair[A, B], s: FindSingle[A]): FindPair[A, B] = AndLeft(p, s)

    override def chain[A: SchemaObject, B: SchemaObject, C: SchemaObject](p: FindPair[A, B], q: FindPair[B, C]): FindPair[A, C] = Chain(p, q)

    override def id[A: SchemaObject]: FindPair[A, A] = FindIdentity()

    override def distinct[A: SchemaObject, B: SchemaObject](p: FindPair[A, B]): FindPair[A, B] = Distinct(p)

    override def exactly[A: SchemaObject](p: FindPair[A, A], n: Int): FindPair[A, A] = Exactly(n, p)

    override def upto[A: SchemaObject](p: FindPair[A, A], n: Int): FindPair[A, A] = Upto(n, p)

    override def readPair[A: SchemaObject, B: SchemaObject](p: FindPair[A, B]): Operation[Err, Set[(A, B)]] =
      d.executor.findPairs(p)

    override def readSingle[A: SchemaObject](s: FindSingle[A]): Operation[Err, Set[A]] =
      d.executor.find(s)

    override def shortestPath[A: SchemaObject](start: A, end: A, p: FindPair[A, A]): Operation[Err, Option[Path[A]]] =
      d.executor.shortestPath(start, end, p)

    override def allShortestPaths[A: SchemaObject](start: A, p: FindPair[A, A]): Operation[Err, Set[Path[A]]] =
      d.executor.allShortestPaths(start, p)

    override def insert[A: SchemaObject, B: SchemaObject](relations: Seq[R[A, B]]): Op[Unit] =
      d.executor.insert(relations.map { case (a, r, b) => CompletedRelation(a, r, b) })

    override def equalResult[A: SchemaObject](s: Set[A], t: Set[A]): Op[Boolean] = m.point(s == t)

    override def equalResult[A: SchemaObject, B: SchemaObject](s: Set[(A, B)], t: Set[(A, B)]): Op[Boolean] = m.point(s == t)
  }
}

object PartII {
  def relationToFindPair[A: SchemaObject, B: SchemaObject](relation: Relation[A, B]): FindPair[A, B] = Rel(relation)

  def objectToFindSingle[A: SchemaObject](a: A): FindSingle[A] = Find(implicitly[SchemaObject[A]].findable(a))

  implicit def r[A: SchemaObject, B: SchemaObject](r: Relation[A, B]): FindPair[A, B] = relationToFindPair(r)

  implicit def o[A: SchemaObject](a: A): FindSingle[A] = objectToFindSingle(a)
}