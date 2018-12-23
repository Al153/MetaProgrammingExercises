package impl.bytecode.implementation

import impl.bytecode.implementation.values.StackValue

/**
  * A procedure simply modifies the stack.
  * @param p
  */
case class Procedure(p: List[StackValue] => List[StackValue])
