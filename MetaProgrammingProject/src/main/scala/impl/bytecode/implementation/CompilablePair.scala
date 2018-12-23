package impl.bytecode.implementation

import impl.bytecode.implementation.values.{PairRelation, StackValue}

/**
  * A simple class which allows a pair of compilable values to be extracted from a stackValue
  * @tparam A result type
  * @tparam B result type
  */
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
