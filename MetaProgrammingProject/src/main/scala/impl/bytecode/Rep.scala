package impl.bytecode

import impl.bytecode.values.StackValue

class Rep[A](bytecode: Vector[Bytecode], extract: StackValue => A) {
  def run(): A = {

    val result = BytecodeInterpreter.interpret(bytecode)

    println("Result = " + result)
    extract(result)
  }
}