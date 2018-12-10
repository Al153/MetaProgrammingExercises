package forth.virtual.machine

sealed trait FVMValue[+R]

case class I(i: Int) extends FVMValue[Nothing]

case class B(b: Boolean) extends FVMValue[Nothing]

case class T(s: String) extends FVMValue[Nothing]

case class Op(b: ByteCode) extends FVMValue[Nothing]

case object N extends FVMValue[Nothing]

case class Ref[R](r: R) extends FVMValue[R]