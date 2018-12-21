package impl.bytecode


sealed trait Bytecode[+Label, +Procedure]

case object OrB extends Bytecode[Nothing, Nothing]

case object AndB extends Bytecode[Nothing, Nothing]

case object AndLB extends Bytecode[Nothing, Nothing]

case object FromB extends Bytecode[Nothing, Nothing]

case object AndRB extends Bytecode[Nothing, Nothing]

case class Call[Procedure](p: Procedure) extends Bytecode[Nothing, Procedure]

case object RotateBack3 extends Bytecode[Nothing, Nothing]

case object Swap extends Bytecode[Nothing, Nothing]

case object RotateForward3 extends Bytecode[Nothing, Nothing]

case object Over extends Bytecode[Nothing, Nothing]

case object Dup extends Bytecode[Nothing, Nothing]

case object JoinB extends Bytecode[Nothing, Nothing]

case class Test[Label](label: Label) extends Bytecode[Label, Nothing] // if =0, jump and drop, else don't drop, continue

case class Jump[Label](label: Label) extends Bytecode[Label, Nothing]

case class TestNotEqual[Label](label: Label) extends Bytecode[Label, Nothing]

case class Push(i: Int) extends Bytecode[Nothing, Nothing]

case object DisB extends Bytecode[Nothing, Nothing]

case object Drop extends Bytecode[Nothing, Nothing]

case class MarkLabel[Label](label: Label) extends Bytecode[Label, Nothing]

case object RevB extends Bytecode[Nothing, Nothing]






