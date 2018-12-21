package impl.bytecode.implementation

import impl.bytecode.implementation.values.StackValue
import impl.bytecode.{BytecodeImpl, Interpreter}
import impl.common._

object SimpleImplementation extends BytecodeImpl[Id, Set, Relation, Set, Label, Procedure, Compilable] {
  override type Result = StackValue




  override def interpreter: Interpreter[Id, StackValue, Label, Procedure] = BytecodeInterpreter

  override def extractPair[A: Compilable, B: Compilable](p: StackValue): Id[Set[(A, B)]] =
    new CompilablePair[A, B]().extract(p)

  override def extractSingle[A: Compilable](result: StackValue): Id[Set[A]] =
    implicitly[Compilable[A]].extract(result)
}
