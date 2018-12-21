package impl.bytecode

import impl.bytecode.values.StackValue

case class Procedure(p: List[StackValue] => List[StackValue])

