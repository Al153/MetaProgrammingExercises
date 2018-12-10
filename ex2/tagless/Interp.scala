package tagless

import tagless.TaglessFinal.DSL

/**
  * The interpret function is essentially a special case of the Reader[Int, A] Monad
  */
case class Interp[A](f: Int => A)

object Interp {

  /**
    * Define an interpreted DSL Instance
    */
  implicit object InterpretedDSL extends DSL[Interp, MatrixPicture] {
    override def int(i: Int): Interp[Int] = Interp(_ => i)

    override def it: Interp[Int] = Interp(i => i)

    override def plus(a: Interp[Int], b: Interp[Int]): Interp[Int] = Interp(i => a.f(i) + b.f(i))

    override def minus(a: Interp[Int], b: Interp[Int]): Interp[Int] = Interp(i => a.f(i) - b.f(i))

    override def times(a: Interp[Int], b: Interp[Int]): Interp[Int] = Interp(i => a.f(i) * b.f(i))

    override def lt(a: Interp[Int], b: Interp[Int]): Interp[Boolean] = Interp(i => a.f(i) < b.f(i))

    override def gt(a: Interp[Int], b: Interp[Int]): Interp[Boolean] = Interp(i => a.f(i) > b.f(i))

    override def if_[A](c: Interp[Boolean], t: Interp[A], e: Interp[A]): Interp[A] = Interp(i => if (c.f(i)) t.f(i) else e.f(i))

    override def run(program: Interp[Int], picture: MatrixPicture): MatrixPicture = MatrixPicture(picture.array.map(_.map(program.f)))
  }

  /**
    * Demo
    */
  def main(args: Array[String]): Unit = {
    val fade = MatrixPicture(Vector(Vector(1, 2, 3), Vector(2, 3, 4), Vector(3, 4, 5)))

    val expected = MatrixPicture(Vector(Vector(3, 3, 3), Vector(3, 3, 7), Vector(3, 7, 8)))

    import InterpretedDSL._
    import TaglessFinal._

    val prog = ((it < 4) ? (0.lift, it)) + 3


    print(run(prog, fade))
    assert(run(prog, fade) == expected)


    val D = DefaultThresholdImplementation.Default(InterpretedDSL)


    val expectedThreshold = MatrixPicture(Vector(Vector(2, 2, 3), Vector(2, 3, 4), Vector(3, 4, 4)))

    val thresholdProg = D.min(D.max(it, 2.lift), 4.lift)

    print(run(thresholdProg, fade))
    assert(run(thresholdProg, fade) == expectedThreshold)
  }

}
