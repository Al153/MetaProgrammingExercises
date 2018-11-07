package forth.virtual.machine

import scala.language.higherKinds


trait ForthSystem {
  type Comp[+A]
  type R

  implicit def witness: IsComp[Comp]

  def pushValue(i: FVMValue[R]): Comp[Unit]

  def pushCont(i: FVMValue[R]): Comp[Unit]

  def popValue: Comp[FVMValue[R]]

  def popCont: Comp[FVMValue[R]]

  def doReturn: Comp[Unit]

  def setMode(i: Int): Comp[Unit]

  def getMode: Comp[Int]

  def deref(r: R): Comp[Rec[R]]

  def setRef(ref: R, rec: Rec[R]): Comp[Unit]

  def getDict: Comp[Ref[R]]

  def setDict(d: Ref[R]): Comp[Unit]

  def createRef(r: Rec[R]): Comp[Ref[R]]

  def printValue(v: FVMValue[R]): Comp[Unit]

  def nextToken: Comp[T]

  def nextChar: Comp[T]

  def getField(ref: R, field: Int): Comp[FVMValue[R]]

  def setField(ref: R, field: Int, value: FVMValue[R]): Comp[Unit]
}

/**
  * The implementation-independent funcitons
  */
trait ForthCore {
  self: ForthSystem =>
  def eval(node: Rec[R]): Comp[Unit] =
    node match {
      case ArgInstr(FPush, v: FVMValue[R], rest) => pushValue(v) andThen continue(rest)

      case Instr(FPlus, rest) => binOp { case (I(a), I(b)) => List(I(a + b)) }.andThen(continue(rest))
      case Instr(FSub, rest) => binOp { case (I(a), I(b)) => List(I(b - a)) }.andThen(continue(rest))

      case ArgInstr(FBranch, alternative: FVMValue[R], default: FVMValue[R]) =>
        popValue.flatMap {
          case B(true) => continue(alternative)
          case B(false) => continue(default)
        }

      case Instr(FReturn, _) => doReturn

      case Instr(FSetMode, rest) => popValue.flatMap { case I(a) => setMode(a) }.andThen(continue(rest))

      case Instr(FGetMode, rest) => getMode.flatMap((i: Int) => pushValue(I(i))).andThen(continue(rest))

      case Instr(FAlloc, rest) => popValue.flatMap { case I(size) => allocate(size) } flatMap pushValue andThen continue(rest)

      case Instr(FGetDict, rest) => getDict flatMap pushValue andThen continue(rest)
      case Instr(FSetDict, rest) => popValue flatMap { case r: Ref[R] => setDict(r) } andThen continue(rest)

      case Instr(FSetField, rest) =>
        popValue flatMap {
          case I(fieldIndex) =>
            popValue flatMap {
              case Ref(r) =>
                popValue flatMap (value => setField(r, fieldIndex, value))
            }
        } andThen continue(rest)
      case Instr(FGetField, rest) =>
        popValue flatMap {
          case I(field) =>
            popValue flatMap {
              case Ref(r) =>
                getField(r, field) flatMap pushValue
            }
        } andThen continue(rest)

      case Instr(FCall, rest) => pushCont(rest).andThen(popValue).flatMap(subroutine => continue(subroutine))

      case Instr(FInWord, rest) => nextToken flatMap pushValue andThen continue(rest)
      case Instr(FInWord, rest) => nextChar flatMap pushValue andThen continue(rest)

      case Instr(FIsInt, rest) => popValue flatMap { case T(s) => witness.unit(B(toInt(s).nonEmpty)) } flatMap pushValue andThen continue(rest)

      case Instr(FConvertInt, rest) => popValue flatMap { case T(s) => witness.unit(I(s.toInt)) } flatMap pushValue andThen continue(rest)

      case Instr(FConcat, rest) => binOp { case (T(s1), T(s2)) => Seq(T(s1 + s2)) } andThen continue(rest)

      case Instr(FPrint, rest) => popValue flatMap printValue andThen continue(rest)

      case Instr(FDrop, rest) => binOp { case (a, b) => Seq(b) } andThen continue(rest)

      case Instr(FDup, rest) => binOp { case (a, b) => Seq(b, a, a) } andThen continue(rest)

      case Instr(FSwap, rest) => binOp { case (a, b) => Seq(a, b) } andThen continue(rest)
    }

  // can be overidden
  def pushMultiValue(s: Seq[FVMValue[R]]): Comp[Unit] =
    s.foldLeft[Comp[Unit]](witness.unit(())) { case (u, v) => u andThen pushValue(v) }

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

  /**
    * Allocate and return a ref to a new record of the right size
    *
    * @param size
    * @return
    */
  def allocate(size: Int): Comp[Ref[R]] = createRef(new Rec[R](Vector.fill(size)(N)))

  def binOp(f: (FVMValue[R], FVMValue[R]) => Seq[FVMValue[R]]): Comp[Unit] =
    popValue.flatMap(a => popValue.flatMap(b => pushMultiValue(f(a, b))))

  def continue(c: FVMValue[R]): Comp[Unit] = c match {
    case N => doReturn
    case Ref(r) => deref(r).flatMap(eval)
  }
}