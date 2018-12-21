package impl.bytecode

import query.dsl.components.Monad

abstract class Interpreter[M[_] : Monad, Returns, Label, Procedure] {
  def interpret(program: Vector[Bytecode[Label, Procedure]]): M[Returns]
}
