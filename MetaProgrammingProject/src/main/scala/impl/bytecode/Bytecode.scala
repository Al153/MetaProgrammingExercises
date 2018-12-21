package impl.bytecode

import impl.bytecode

sealed trait Bytecode

case object OrB extends Bytecode

case object AndB extends Bytecode

case object AndLB extends Bytecode

case object FromB extends Bytecode

case object AndRB extends Bytecode

case class Call(p: Procedure) extends Bytecode

case object RotateBack3 extends Bytecode

case object Swap extends Bytecode

case object RotateForward3 extends Bytecode

case object Over extends Bytecode

case object Dup extends Bytecode

case object JoinB extends Bytecode

case class Test(label: bytecode.Label) extends Bytecode // if =0, jump and drop, else don't drop, continue

case class Jump(label: bytecode.Label) extends Bytecode

case class TestNotEqual(label: Label) extends Bytecode

case class Push(i: Int) extends Bytecode

case object DisB extends Bytecode

case object Drop extends Bytecode

case class MarkLabel(label: bytecode.Label) extends Bytecode

case object RevB extends Bytecode






