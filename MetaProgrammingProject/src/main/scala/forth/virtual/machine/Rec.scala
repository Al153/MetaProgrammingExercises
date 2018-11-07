package forth.virtual.machine

/**
  * Record class needs specialised constructors
  *
  * @param fields
  */
case class Rec[R](fields: Vector[FVMValue[R]])

object RecSeq {
  def apply[R](fields: FVMValue[R]*): Rec[R] = Rec(fields.toVector)
  def unapplySeq[R](arg: Rec[R]): Option[Seq[FVMValue[R]]] = Some(arg.fields)
}

object ArgInstr {
  def unapply[R](n: Rec[R]): Option[(ByteCode, FVMValue[R], FVMValue[R])] =
    n match {
      case RecSeq(Op(b), arg: FVMValue[R], rest) => Some((b, arg, rest))
      case _ => None
    }
}

object Instr {

  def unapply[R](n: Rec[R]): Option[(ByteCode, FVMValue[R])] =
    n match {
      case RecSeq(Op(b), rest) => Some((b, rest))
      case _ => None
    }
}