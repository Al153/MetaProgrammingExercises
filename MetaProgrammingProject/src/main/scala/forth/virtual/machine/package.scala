package forth.virtual


/**
  * Game Plan
  *
  * Define Low level VM (Fully monadic?)
  * Define scala DSL for building FVM programs
  * Build a minimal forth using the DSL
  * Expand the forth from forth
  * Add a new metalayer to the forth from forth
  *   - E.g. cross compilation layer to compile the current forth to a lower level VM
  */

package object machine {

  sealed trait FVMValueStack[R] {
    def stack: List[FVMValue[R]]
  }

  sealed trait FVMContinuationStack[R] {
    def stack: List[FVMValue[R]]

  }


  class Setup(forthSystem: ForthSystem with ForthCore) {

    import forthSystem._

    /**
      * Dict structure:
      *
      * Forth-List of Frames
      * Frame is: Forth-List of Word Records
      */

    def wordRecord(name: String, modeDefinitions: List[FVMValue[R]]): Comp[FVMValue[R]] =
      for {
        r <- createRef(Rec(modeDefinitions.toVector))
        res <- createRef(RecSeq(T(name), r))
      } yield res


    def createDictFrame(defs: List[(String, List[FVMValue[R]])]): Comp[FVMValue[R]] =
      for {
        refs <- sequence(defs, {
          case (name, modeDefs) => wordRecord(name, modeDefs)
        })
        res <- createRef(Rec(refs.toVector))
      } yield res


    def sequence[A, B](as: Seq[A], f: A => Comp[B]): Comp[Seq[B]] =
      as.foldRight(witness.unit[Seq[B]](List.empty[B])) {
        case (a, cbs) =>
          for {
            bs <- cbs
            b <- f(a)
          } yield b +: bs
      }


    def createNode(b: ByteCode, rest: Unit => Comp[Option[Ref[R]]]): Comp[Ref[R]] = rest().flatMap {
      case Some(r) => createRef(RecSeq(Op(b), r))
      case _ => createRef(RecSeq(Op(b), N))
    }

    def createNode(b: ByteCode, arg: FVMValue[R], rest: Unit => Comp[Option[Ref[R]]]): Comp[Ref[R]] = rest().flatMap {
      case Some(r) => createRef(RecSeq(Op(b), arg, r))
      case _ => createRef(RecSeq(Op(b), arg, N))
    }


    type Continuation[A] = Ref[R] => Comp[Ref[R]] // (Leaves an A at the top of the stack)
    def if_[A](c: Continuation[Boolean], t: Continuation[A], f: Continuation[A]): Continuation[A] =
      (next: Ref[R]) => for {
        trueClause <- t(next)
        ifClause <- createNode(FBranch, trueClause, _ => f(next).map(Some(_)))
        res <- c(ifClause)
      } yield res


  }

  sealed trait FVMState[R] {
    def vStack: FVMValueStack[R]

    def cStack: FVMContinuationStack[R]

    def dict: Ref[R]

    def mode: FMode
  }

  case class FMode(mode: Int)


  // abstract computation monad
  trait IsComp[M[_]] {
    def unit[A](a: => A): M[A]

    def bind[A, B](ma: M[A], f: A => M[B]): M[B]

    def run[A](ma: M[A]): Unit
  }

  implicit class CompOps[M[_] : IsComp, A](ma: M[A]) {
    val C = implicitly[IsComp[M]]

    def map[B](f: A => B): M[B] = C.bind(ma, (a: A) => C.unit(f(a)))

    def flatMap[B](f: A => M[B]): M[B] = C.bind(ma, f)

    def andThen[B](that: M[B]): M[B] = C.bind(ma, (_: A) => that)
  }

}
