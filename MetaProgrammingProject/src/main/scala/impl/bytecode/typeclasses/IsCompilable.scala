package impl.bytecode.typeclasses

import scala.language.higherKinds

/**
  * Ensures that a "compilable" typeclass C can provide an Identity procedure.
  * @tparam C - A typeclass
  * @tparam Proc - procedure type
  */
trait IsCompilable[C[_], Proc] {
  def getId[A: C]: Proc
}
