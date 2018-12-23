package impl.bytecode

import query.dsl.components.Monad

import scala.language.higherKinds

/**
  * A simple non-tagless-final interpreter interface.
  * @tparam M - Computation monad
  * @tparam Returns - result of the computation
  * @tparam Label - Labels in the bytecode
  * @tparam Procedure - Procedures in the bytecode
  */
abstract class Interpreter[M[_] : Monad, Returns, Label, Procedure] {

  /**
    * Interpret a program.
    * @param program
    */
  def interpret(program: Vector[Bytecode[Label, Procedure]]): M[Returns]
}
