package tagless

import scalaz.State
import tagless.TaglessFinal.DSL

/**
  * A DSL implementation that compiles DSL programs to a program for a simple stack machine.
  * the stack machine's bytecode is then compiled to State monad instance to be run on each pixel individually.
  */
object StackMachine {

  /**
    * Bytecode instructions
    */
  sealed trait ByteCode

  case class PushInt(i: Int) extends ByteCode
  case object Add extends ByteCode
  case object Subtract extends ByteCode
  case object Multiply extends ByteCode
  case object LT extends ByteCode
  case object GT extends ByteCode
  case object Max extends ByteCode
  case object Min extends ByteCode

  /**
    * The branch bytecode stores branches as a tree rather than offset jumps, for simplicity
    */
  case class Branch(t: Vector[ByteCode], f: Vector[ByteCode]) extends ByteCode
  case object PushIt extends ByteCode

  /**
    * Values allows on the stack
    */
  sealed trait Val
  case class I(i: Int) extends Val
  case class B(b: Boolean) extends Val

  /**
    * Type definitions of Stack machine state
    *
    * If I were builing a robust system, the underlying monad would be something along the lines of
    * type Computation[A] = EitherT[StateT[ReaderT[IO, ?], Stack, ?], E, A]
    */
  type Stack = List[Val]
  type VMState[A] = State[Stack, A]
  type Program[A] = Vector[ByteCode]


  /**
    * Utility function for lifting State transitions to monad instances.
    */
  def stateTransformation(f: Stack => Stack): VMState[Unit] = State[Stack, Unit](s => (f(s), ()))

  /**
    * Since the program that runs needs to reference the original value, we actually need a StateT[Reader[Int, ?], Stack, A].
    * This is achieved by using an (Int => State[Stack, A])
    */

  /**
    * Compile bytecode to a State monad instance.
    */
  def compileBytecode(b: ByteCode): Int => VMState[Unit] =
    originalValue => b match {

      case PushInt(i) => stateTransformation(s => I(i) :: s)
      case PushIt => stateTransformation(s => I(originalValue) :: s)
      case Add => stateTransformation {
        case I(a1) :: I(b1) :: rest => I(a1 + b1) :: rest
        case s => throw StackError(b, s)
      }

      case Subtract => stateTransformation {
        case I(a1) :: I(b1) :: rest => I(b1 - a1) :: rest
        case s => throw StackError(b, s)
      }
      case Multiply => stateTransformation {
        case I(a1) :: I(b1) :: rest => I(a1 * b1) :: rest
        case s => throw StackError(b, s)
      }
      case LT => stateTransformation {
        case I(a1) :: I(b1) :: rest => B(b1 < a1) :: rest
        case s => throw StackError(b, s)
      }
      case GT => stateTransformation {
        case I(a1) :: I(b1) :: rest => B(b1 > a1) :: rest
        case s => throw StackError(b, s)
      }

      case Min => stateTransformation {
        case I(a1) :: I(b1) :: rest => I(if (a1 > b1) b1 else a1) :: rest
        case s => throw StackError(b, s)
      }

      case Max => stateTransformation {
        case I(a1) :: I(b1) :: rest => I(if (a1 < b1) b1 else a1) :: rest
        case s => throw StackError(b, s)
      }

      case Branch(t, f) => State[Stack, Boolean] {
        case B(c) :: rest => (rest, c)
        case s => throw StackError(b, s)
        // inefficient - compiles branches at run time, since the branches aren't inlined
      } flatMap (b => if (b) compile(t)(originalValue) else compile(f)(originalValue))
    }

  // Under a fully safe system, this would be incorporated into an EitherT monad stack.
  case class StackError(b: ByteCode, stack: Stack) extends Throwable

  // compile a whole program to a State monad instance
  def compile(p: Program[_]): Int => VMState[Unit] =
    i =>
      p.foldLeft[VMState[Unit]](State.state(())) {
        case (comp, bytecode) => comp.flatMap(_ => compileBytecode(bytecode)(i))
      }

  /**
    *  DSL Typleclass instance. The stack machine has special instructions for Min and Max and hence can implement
    *  them natively rather than using the free implementations
    */
  implicit object StackDSL extends DSL[Program, MatrixPicture] with Thresholding[Program, MatrixPicture]{
    override def int(i: Int): Program[Int] = Vector(PushInt(i))

    override def it: Program[Int] = Vector(PushIt)

    override def plus(a: Program[Int], b: Program[Int]): Program[Int] = a ++ b :+ Add

    override def minus(a: Program[Int], b: Program[Int]): Program[Int] = a ++ b :+ Subtract

    override def times(a: Program[Int], b: Program[Int]): Program[Int] = a ++ b :+ Multiply

    override def lt(a: Program[Int], b: Program[Int]): Program[Boolean] = a ++ b :+ LT

    override def gt(a: Program[Int], b: Program[Int]): Program[Boolean] = a ++ b :+ GT

    override def if_[A](c: Program[Boolean], t: Program[A], e: Program[A]): Program[A] = c :+ Branch(t, e)

    override def run(program: Program[Int], picture: MatrixPicture): MatrixPicture = {
      /**
        * Compile the program into a Int => Int
        */
      val toRun: Int => Int = in => compile(program)(in).run(List.empty)._1.head match {
        case I(i) => i
      }

      /**
        * Map the program function over the picture.
        */
      MatrixPicture(picture.array.map(_.map(toRun)))


    }

    /**
      * Max of two values
      */
    override def max(a: Program[Int], b: Program[Int]): Program[Int] = a ++ b :+ Max

    /**
      * Min of two values
      */
    override def min(a: Program[Int], b: Program[Int]): Program[Int] = a ++ b :+ Min
  }

}

/**
  * Demo of the stack machine based system.
  */
object StackMain {
  def main(args: Array[String]): Unit = {
    val fade = MatrixPicture(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(3, 4, 5)))

    val expected = MatrixPicture(Vector(Vector(3, 3, 3), Vector(3, 3, 7), Vector(3, 7, 8)))

    import TaglessFinal._
    import tagless.StackMachine._
    import StackDSL._


    val prog: Program[_] = ((it < 4) ? (0.lift, it)) + 3


    print(run(prog, fade))
    assert(run(prog, fade) == expected)



    val thresholdProg = min(max(it, 2.lift), 4.lift)

    val expectedThreshold = MatrixPicture(Vector(Vector(2, 2, 3), Vector(2, 3, 4), Vector(3, 4, 4)))

    print(run(thresholdProg, fade))
    assert(run(thresholdProg, fade) == expectedThreshold)
  }
}
