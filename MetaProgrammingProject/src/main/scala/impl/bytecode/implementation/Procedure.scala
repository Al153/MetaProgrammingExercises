package impl.bytecode.implementation

import impl.bytecode.implementation.values.StackValue

case class Procedure(p: List[StackValue] => List[StackValue])
