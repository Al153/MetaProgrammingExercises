package impl.bytecode.implementation

import impl.bytecode.implementation.values.PairRelation

/**
  * Class which relations inherit from. Provides procedure for pushing the relation onto the stack.
  */

abstract class Relation[A: Compilable, B: Compilable] {
  def pairs: Set[(A, B)]

  private val CA = implicitly[Compilable[A]]
  private val CB = implicitly[Compilable[B]]

  final lazy val procedure: Procedure = {
    val pr = PairRelation(for ((a, b) <- pairs) yield CA.convert(a) -> CB.convert(b))

    Procedure(s => pr :: s)
  }
}