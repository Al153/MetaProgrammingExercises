package impl.bytecode.values

sealed trait StackValue

case class PairRelation(p: Set[(StackObject, StackObject)]) extends StackValue

case class SingleRelation(s: Set[StackObject]) extends StackValue

case class Integer(i: Int) extends StackValue
