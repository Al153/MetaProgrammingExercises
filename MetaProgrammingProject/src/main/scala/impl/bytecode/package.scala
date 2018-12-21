package impl

import impl.bytecode.erased.adt.{P, Prim, PrimS, S}

import scala.language.implicitConversions

package object bytecode {
  type Find[A] = Set[A]

  /**
    * Type-erased P
    */


  implicit class RelationOps[A: Compilable, B: Compilable](r: Relation[A, B]) {
    def p: Pr[A, B] = Pr(Prim(r.procedure))
  }

  implicit class ObjectOps[A: Compilable](a: A) {
    def o: Sn[A] = Sn(PrimS(implicitly[Compilable[A]].toProc(a)))
  }

  implicit class FindableOps[A: Compilable](fa: Find[A]) {
    def f: Sn[A] = BytecodeCompiler.find(fa)
  }

  case class Pr[A: Compilable, B: Compilable](p: P)

  case class Sn[A: Compilable](s: S)

}
