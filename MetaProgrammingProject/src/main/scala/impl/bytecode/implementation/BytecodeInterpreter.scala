package impl.bytecode.implementation

import impl.bytecode.implementation.values.{Integer, PairRelation, SingleRelation, StackValue}
import impl.bytecode._
import impl.common.{Id, joinSet, _}

/**
  * A simple, imperative, side-effectful bytecode interpreter, based on a stack machine.
  */
object BytecodeInterpreter extends
  Interpreter[Id, StackValue, Label, Procedure] {

  /**
    * Interprets a program then returns the top stack value
    *
    * @param program the bytecode to interpret
    * @return The top stack value on the left over stack
    */
  def interpret(program: Vector[Bytecode[Label, Procedure]]): StackValue = {

    /**
      * Build an index of all the label locations in the program in order to effect the jumps
      */
    val mapBuilder = Map.newBuilder[Label, Int]


    for ((bytecode, index) <- program.zipWithIndex) {
      bytecode match {
        case MarkLabel(l) => mapBuilder += l -> index
        case _ => ()
      }
    }

    val labelMap = mapBuilder.result()

    /**
      *  Set up local variables
      */
    var programCounter = 0
    var stack = List.empty[StackValue]

    /**
      * Simple interpretation loop; halts when the program counter points after the end of the program
      */
    while (programCounter < program.length) {
      var pcChanged = false
      program(programCounter) match {
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
          case SingleRelation(s) :: PairRelation(p) :: rest => PairRelation(p filter { case (a, _) => s.contains(a) }) :: rest
        }
        case FromB => stack = stack match {
          case PairRelation(p) :: SingleRelation(s) :: rest => SingleRelation(p collect { case (a, b) if s.contains(a) => b }) :: rest
        }
        case AndRB => stack = stack match {
          case SingleRelation(s) :: PairRelation(p) :: rest => PairRelation(p filter { case (_, b) => s.contains(b) }) :: rest
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
          case PairRelation(q) :: PairRelation(p) :: rest => PairRelation(joinSet(p, q)) :: rest
        }
        case TestAndDecrement(label) => stack match {
          case Integer(n) :: rest if n <= 0 =>
            stack = rest
            programCounter = labelMap(label)
            pcChanged = true

          case Integer(n) :: rest if n > 0 =>
            stack = Integer(n - 1) :: rest
        }
        case Jump(label) =>
          programCounter = labelMap(label)
          pcChanged = true
        case TestNotEqual(label) =>
          stack match {
            case a :: b :: rest =>
              if (a == b) {
                stack = rest
              } else {
                stack = rest
                programCounter = labelMap(label)
                pcChanged = true
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

      if (!pcChanged) {
        programCounter += 1
      }
    }

    stack.head
  }
}
