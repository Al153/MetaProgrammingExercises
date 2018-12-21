package impl.bytecode

import query.dsl.components._
import impl.bytecode.erased.adt._


object BytecodeCompiler
  extends PairQueries[Pr, Sn, Compilable]
    with SingleQueries[Pr, Sn, Find, Compilable]
    with Reads[Rep, Set, Pr, Sn, Compilable]
    with PairSyntaxProvider[Pr, Sn, Compilable]
    with SingleSyntaxProvider[Pr, Sn, Find, Compilable]
    with SymmetricSyntaxProvider[Pr, Sn, Compilable] {
  /**
    * @return find a findable
    */
  override def find[A: Compilable](f: Find[A]): Sn[A] = Sn(PrimS(implicitly[Compilable[A]].toProc(f)))

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
  override def exactly[A: Compilable](p: Pr[A, A], n: Int): Pr[A, A] = Pr(Exactly(p.p, n, implicitly[Compilable[A]].id))

  /**
    * @return the union of the relations
    */
  override def or[A: Compilable, B: Compilable](p: Pr[A, B], q: Pr[A, B]): Pr[A, B] = Pr(OrP(p.p, q.p))

  /**
    * @return the identity relation
    */
  override def id[A: Compilable]: Pr[A, A] = Pr(Id(implicitly[Compilable[A]].id))

  /**
    * @param p base query
    * @return the fixed point of appending p to itself
    */
  override def fixedPoint[A: Compilable](p: Pr[A, A]): Pr[A, A] = Pr(Fix(p.p, implicitly[Compilable[A]].id))

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

  override def readPair[A: Compilable, B: Compilable](p: Pr[A, B]): Rep[Set[(A, B)]] = {
    println(s"CA.U = ${implicitly[Compilable[A]].universe}")
    println(s"CB.U = ${implicitly[Compilable[B]].universe}")

    new Rep(compile(p.p), new CompilablePair[A, B].extract)
  }

  override def readSingle[A: Compilable](s: Sn[A]): Rep[Set[A]] =
    new Rep(compile(s.s), implicitly[Compilable[A]].extract)

  def compile(pr: P): Vector[Bytecode] =
    pr match {
      case Prim(p) => Vector(Call(p))
      case AndP(p, q) => compile(p) ++ compile(q) :+ OrB
      case Distinct(p) => compile(p) :+ DisB
      case Id(p) => Vector(Call(p))
      case Exactly(p, n, id) =>
        if (n < 0) compile(Exactly(Rev(p), n, id)) else if (n == 0) Vector(Call(id))
        else if (n >= 1 && n <= 5) {
          // simple loop unrolling
          compile(p) ++ Vector.fill(n - 1)(Dup) ++ Vector.fill(n - 1)(JoinB)
        } else {
          // can't unroll. naive linear interpretation
          val end: Label = Label.newLabel()
          val start: Label = Label.newLabel()
          compile(p) ++ Vector( // ( p )
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
      case OrP(p, q) => compile(p) ++ compile(q) :+ OrB
      case Join(p, q) => compile(p) ++ compile(q) :+ JoinB
      case AndLeft(p, s) => compile(p) ++ compile(s) :+ AndLB
      case AndRight(p, s) => compile(p) ++ compile(s) :+ AndRB
      case Rev(p) => compile(p) :+ RevB
      case Fix(p, id) =>
        val start: Label = Label.newLabel()
        compile(p) ++ Vector(
          Call(id), // (p id)
          OrB, // (q); q = p join id
          MarkLabel(start),
          Dup, // (q q)
          Dup, // (q q q)
          JoinB, // (q, 2q)
          Dup, // (q, 2q, 2q)
          RotateForward3, // (2q, 2q, q)
          TestNotEqual(start) // if equal, drop both, continue. if not equal, drop both, jump
        )
    }

  def compile(si: S): Vector[Bytecode] =
    si match {
      case PrimS(p) => Vector(Call(p))
      case OrS(s, t) => compile(s) ++ compile(t) :+ OrB
      case AndS(s, t) => compile(s) ++ compile(t) :+ AndB
      case From(s, p) => compile(s) ++ compile(p) :+ FromB
    }


}


