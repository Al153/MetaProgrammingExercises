package impl.bytecode.implementation

import impl.bytecode.implementation.values.{PairRelation, StackValue}

class CompilablePair[A: Compilable, B: Compilable] {
  private val CA = implicitly[Compilable[A]]
  private val CB = implicitly[Compilable[B]]

  def extract(s: StackValue): Set[(A, B)] = {
    s match {
      case PairRelation(p) =>
        for ((l, r) <- p) yield {
          (CA.extract(l), CB.extract(r))
        }
    }
  }
}
