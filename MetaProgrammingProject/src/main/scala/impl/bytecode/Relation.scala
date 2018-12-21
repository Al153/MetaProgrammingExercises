package impl.bytecode

import impl.bytecode.values.PairRelation

abstract class Relation[A: Compilable, B: Compilable] {
  def pairs: Set[(A, B)]

  private val CA = implicitly[Compilable[A]]
  private val CB = implicitly[Compilable[B]]

  lazy val procedure: Procedure = {
    val pr = PairRelation(for ((a, b) <- pairs) yield CA.convert(a) -> CB.convert(b))

    Procedure(s => pr :: s)
  }
}