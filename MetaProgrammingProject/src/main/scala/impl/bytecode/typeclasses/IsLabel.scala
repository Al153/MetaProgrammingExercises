package impl.bytecode.typeclasses

import scala.language.higherKinds


/**
  * Typeclass ensuring that new labels can be generated.
  * @tparam L - A label type
  * @tparam M - a (possibly monadic) computation type.
  */
trait IsLabel[L, M[_]] {
  def newLabel(): M[L]
}

