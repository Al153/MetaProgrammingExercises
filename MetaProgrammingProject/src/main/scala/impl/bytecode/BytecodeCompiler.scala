package impl.bytecode

import impl.bytecode.erased.adt._
import query.dsl.components.Monad._
import query.dsl.components._

trait IsCompilable[C[_], Proc] {
  def getId[A: C]: Proc
}

trait IsLabel[L, M[_]] {
  def newLabel(): M[L]
}

abstract class IsProcedure[P, Relation[_, _], Compilable[_], Find[_]] {
  def convertRelation[A: Compilable, B: Compilable](r: Relation[A, B]): P

  def convertObject[A: Compilable](a: A): P

  def convertFindable[A: Compilable](fa: Find[A]): P
}


abstract class BytecodeImpl[M[_], Se[_], Relation[_, _], Find[_], L, Proc, Compilable[_]]
(
  implicit isLabel: IsLabel[L, M],
  MM: Monad[M],
  CC: IsCompilable[Compilable, Proc],
  isProcedure: IsProcedure[Proc, Relation, Compilable, Find]
) {

  type Result

  def extractPair[A: Compilable, B: Compilable](p: Result): M[Se[(A, B)]]

  def extractSingle[A: Compilable](result: Result): M[Se[A]]


  case class Pr[A: Compilable, B: Compilable](p: P[Proc])

  case class Sn[A: Compilable](s: S[Proc])

  def interpreter: Interpreter[M, Result, L, Proc]

  class Rep[A](bytecode: Vector[Bytecode[L, Proc]], extract: Result => M[A]) {
    def run(): M[A] = interpreter.interpret(bytecode).flatMap(extract)
  }

  type ResultMonad[A] = M[Rep[A]]

  class Implementation
    extends PairQueries[Pr, Sn, Compilable]
      with SingleQueries[Pr, Sn, Find, Compilable]
      with Reads[ResultMonad, Se, Pr, Sn, Compilable]
      with PairSyntaxProvider[Pr, Sn, Compilable]
      with SingleSyntaxProvider[Pr, Sn, Find, Compilable]
      with SymmetricSyntaxProvider[Pr, Sn, Compilable] {


    implicit class RelationOps[A: Compilable, B: Compilable](r: Relation[A, B]) {
      def p: Pr[A, B] = Pr(Prim[Proc](isProcedure.convertRelation(r)))
    }

    implicit class ObjectOps[A: Compilable](a: A) {
      def o: Sn[A] = Sn(PrimS(isProcedure.convertObject(a)))
    }

    implicit class FindableOps[A: Compilable](fa: Find[A]) {
      def f: Sn[A] = Sn(PrimS(isProcedure.convertFindable(fa)))
    }


    /**
      * @return find a findable
      */
    override def find[A: Compilable](f: Find[A]): Sn[A] = f.f

    /**
      * @return the intersection of the results of the two subqueries
      */
    override def and[A: Compilable](s: Sn[A], t: Sn[A]): Sn[A] = Sn(AndS(s.s, t.s))

    /**
      * @return the results of following the relation from the start objects
      */
    override def from[A: Compilable, B: Compilable](s: Sn[A], p: Pr[A, B]): Sn[B] = Sn(From(s.s, p.p))

    /**
      * @return Union of the two sub queries
      */
    override def or[A: Compilable](s: Sn[A], t: Sn[A]): Sn[A] = Sn(OrS(s.s, t.s))

    /**
      * @param p query to repeats
      * @param n times
      * @return finds pairs related by up to n repetitions of p
      */
    override def upto[A: Compilable](p: Pr[A, A], n: Int): Pr[A, A] = exactly(or(p, id), n)

    /**
      *
      * @param p query to repeat
      * @param n times
      * @return chains p to itself n times
      */
    override def exactly[A: Compilable](p: Pr[A, A], n: Int): Pr[A, A] = Pr(Exactly(p.p, n, CC.getId(implicitly[Compilable[A]])))

    /**
      * @return the union of the relations
      */
    override def or[A: Compilable, B: Compilable](p: Pr[A, B], q: Pr[A, B]): Pr[A, B] = Pr(OrP(p.p, q.p))

    /**
      * @return the identity relation
      */
    override def id[A: Compilable]: Pr[A, A] = Pr(Id(CC.getId[A]))

    /**
      * @param p base query
      * @return the fixed point of appending p to itself
      */
    override def fixedPoint[A: Compilable](p: Pr[A, A]): Pr[A, A] = Pr(Fix(p.p, CC.getId[A]))

    /**
      * Reverse the direction of the relation
      */
    override def reverse[A: Compilable, B: Compilable](p: Pr[A, B]): Pr[B, A] = Pr(Rev(p.p))

    /**
      * @return the intersection of the relations
      */
    override def and[A: Compilable, B: Compilable](p: Pr[A, B], q: Pr[A, B]): Pr[A, B] = Pr(AndP(p.p, q.p))

    /**
      * @return filter the relation to those that match the single query
      */
    override def andRight[A: Compilable, B: Compilable](p: Pr[A, B], s: Sn[B]): Pr[A, B] = Pr(AndRight(p.p, s.s))

    /**
      * @return filter the relation to those pairs where the left value matches the single query
      */
    override def andLeft[A: Compilable, B: Compilable](p: Pr[A, B], s: Sn[A]): Pr[A, B] = Pr(AndLeft(p.p, s.s))

    /**
      * @return inner join two queries
      */
    override def chain[A: Compilable, B: Compilable, C: Compilable](p: Pr[A, B], q: Pr[B, C]): Pr[A, C] = Pr(Join(p.p, q.p))

    /**
      * @return related values that are not equal
      */
    override def distinct[A: Compilable, B: Compilable](p: Pr[A, B]): Pr[A, B] = Pr(Distinct(p.p))

    override def readPair[A: Compilable, B: Compilable](p: Pr[A, B]): M[Rep[Se[(A, B)]]] = {
      for {
        program <- compile(p.p)
        repResult = new Rep(program, extractPair[A, B])
      } yield repResult
    }

    override def readSingle[A: Compilable](s: Sn[A]): M[Rep[Se[A]]] =
      for {
        program <- compile(s.s)
        repResult = new Rep(program, extractSingle[A])
      } yield repResult

    def compile(pr: P[Proc]): M[Vector[Bytecode[L, Proc]]] =
      pr match {
        case Prim(p) => MM.point(Vector(Call(p)))
        case AndP(p, q) =>
          for {
            pp <- compile(p)
            qq <- compile(q)
          } yield pp ++ qq :+ OrB
        case Distinct(p) => for {pp <- compile(p)} yield pp :+ DisB
        case Id(p) => MM.point(Vector(Call(p): Bytecode[L, Proc]))
        case Exactly(p, n, id) =>
          if (n < 0) compile(Exactly(Rev(p), n, id)) else if (n == 0) MM.point(Vector(Call(id): Bytecode[L, Proc]))
          else if (n >= 1 && n <= 5) {
            // simple loop unrolling
            for {
              pp <- compile(p)
            } yield pp ++ Vector.fill(n - 1)(Dup) ++ Vector.fill(n - 1)(JoinB)
          } else {
            // can't unroll. naive linear interpretation
            for {
              end <- isLabel.newLabel()
              start <- isLabel.newLabel()
              pp <- compile(p)
            } yield pp ++ Vector( // ( p )
              Dup, // ( p r ) r = p
              Push(n), // ( p r n)
              MarkLabel(start), // (p r n)

              Test(end), // (p r n). test(0) -> drop and jump; test(n > 0) -> continue, no drop
              RotateBack3, // ( n p r )
              Over, // (n p r p)
              JoinB, // (n p r.p)
              RotateForward3, // (p r.p n)
              Jump(start),
              MarkLabel(end), // (p res)
              Swap, // (res p)
              Drop // res
            )
          }
        case OrP(p, q) =>
          for {
            pp <- compile(p)
            qq <- compile(q)
          } yield pp ++ qq :+ OrB
        case Join(p, q) =>
          for {
            pp <- compile(p)
            qq <- compile(q)
          } yield pp ++ qq :+ JoinB
        case AndLeft(p, s) =>
          for {
            pp <- compile(p)
            ss <- compile(s)
          } yield pp ++ ss :+ AndLB

        case AndRight(p, s) =>
          for {
            pp <- compile(p)
            ss <- compile(s)
          } yield pp ++ ss :+ AndRB

        case Rev(p) => compile(p).map(pp => pp :+ RevB)
        case Fix(p, id) =>
          isLabel.newLabel().flatMap {
            start =>
              compile(p).map(p => p ++ Vector(
                Call(id), // (p id)
                OrB, // (q); q = p join id
                MarkLabel(start),
                Dup, // (q q)
                Dup, // (q q q)
                JoinB, // (q, 2q)
                Dup, // (q, 2q, 2q)
                RotateForward3, // (2q, 2q, q)
                TestNotEqual(start) // if equal, drop both, continue. if not equal, drop both, jump
              ))
          }
      }

    def compile(si: S[Proc]): M[Vector[Bytecode[L, Proc]]] =
      si match {
        case PrimS(p) => MM.point(Vector(Call(p)))
        case OrS(s, t) => for {
          ss <- compile(s)
          tt <- compile(t)
        } yield ss ++ tt :+ OrB
        case AndS(s, t) =>
          for {
            ss <- compile(s)
            tt <- compile(t)
          } yield ss ++ tt :+ AndB
        case From(s, p) =>
          for {
            ss <- compile(s)
            pp <- compile(p)
          } yield ss ++ pp :+ FromB
      }


  }


}
