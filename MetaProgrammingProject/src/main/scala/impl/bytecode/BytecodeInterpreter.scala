package impl.bytecode

import impl.bytecode.values.{Integer, PairRelation, SingleRelation, StackValue}
import impl.trivial.Utils


object BytecodeInterpreter {
  def interpret(program: Vector[Bytecode]): StackValue = {
    val mapBuilder = Map.newBuilder[Label, Int]
    for ((bytecode, index) <- program.zipWithIndex) {
      bytecode match {
        case MarkLabel(l) => mapBuilder += l -> index
        case _ => ()
      }
    }

    val labelMap = mapBuilder.result()

    var index = 0
    var stack = List.empty[StackValue]

    while (index < program.length) {
      var indexChanged = false

      program(index) match {
        case OrB =>
          stack = stack match {
            case SingleRelation(s) :: SingleRelation(t) :: rest => SingleRelation(s | t) :: rest
            case PairRelation(p) :: PairRelation(q) :: rest => PairRelation(p | q) :: rest
          }
        case AndB =>
          stack = stack match {
            case SingleRelation(s) :: SingleRelation(t) :: rest => SingleRelation(s & t) :: rest
            case PairRelation(p) :: PairRelation(q) :: rest => PairRelation(p & q) :: rest
          }
        case AndLB => stack = stack match {
          case PairRelation(p) :: SingleRelation(s) :: rest => PairRelation(p filter { case (a, _) => s.contains(a) }) :: rest
        }
        case FromB => stack = stack match {
          case PairRelation(p) :: SingleRelation(s) :: rest => SingleRelation(p collect { case (a, b) if s.contains(a) => b }) :: rest
        }
        case AndRB => stack match {
          case PairRelation(p) :: SingleRelation(s) :: rest => PairRelation(p filter { case (_, b) => s.contains(b) }) :: rest
        }
        case Call(p) => stack = p.p(stack)
        case RotateBack3 => stack = stack match {
          case a :: b :: c :: rest => b :: c :: a :: rest
        }
        case Swap => stack = stack match {
          case a :: b :: rest => b :: a :: rest
        }
        case RotateForward3 => stack = stack match {
          case a :: b :: c :: rest => c :: a :: b :: rest
        }
        case Over => stack = stack match {
          case a :: b :: rest => b :: a :: b :: rest
        }
        case Dup => stack = stack match {
          case a :: rest => a :: a :: rest
        }
        case JoinB => stack = stack match {
          case PairRelation(q) :: PairRelation(p) :: rest => PairRelation(Utils.joinSet(p, q)) :: rest
        }
        case Test(label) => stack match {
          case Integer(0) :: rest =>
            stack = rest
            index = labelMap(label)
            indexChanged = true

          case Integer(_) :: _ => ()
        }
        case Jump(label) =>
          index = labelMap(label)
          indexChanged = true
        case TestNotEqual(label) =>
          stack match {
            case a :: b :: rest =>
              if (a == b) {
                stack = rest
              } else {
                stack = rest
                index = labelMap(label)
                indexChanged = true
              }
          }
        case Push(i) =>
          stack = Integer(i) :: stack
        case DisB =>
          stack = stack match {
            case PairRelation(p) :: rest => PairRelation(p filter { case (a, b) => a != b }) :: rest
          }
        case Drop => stack = stack match {
          case _ :: rest => rest
        }
        case MarkLabel(_) => ()
        case RevB => stack = stack match {
          case PairRelation(p) :: rest => PairRelation(p map { case (a, b) => b -> a }) :: rest
        }
      }

      if (!indexChanged) {
        index += 1
      }
    }

    stack.head
  }
}