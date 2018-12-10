package tagless

import scala.language.higherKinds

object TaglessFinal {

  /**
    * Define a Tagless Final DSL typeclass
    */
  trait DSL[Wrapper[_], Picture] {
    /**
      * Puts an int
      */
    def int(i: Int): Wrapper[Int]

    /**
      * Puts the original value
      * @return
      */
    def it: Wrapper[Int]

    /**
      * Add two values
      */
    def plus(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]

    /**
      * Subtract right value from left value
      */
    def minus(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]

    /**
      * Multiply two values
      */
    def times(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]

    /**
      * Compare two values
      */
    def lt(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Boolean]


    /**
      * Compare two values
      */
    def gt(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Boolean]


    /**
      * Branch according to a condition
      */
    def if_[A](c: Wrapper[Boolean], t: Wrapper[A], e: Wrapper[A]): Wrapper[A]

    /**
      * Run a program on a picture
      */
    def run(program: Wrapper[Int], picture: Picture): Picture
  }

  /**
    * Add ternary operator syntax to a boolean DSL instance
    */
  implicit class DSLBoolOps[T[_], P](t: T[Boolean])(implicit D: DSL[T, P]) {
    def ?[A](thenp: T[A], elsep: T[A]): T[A] = D.if_(t, thenp, elsep)
  }

  /**
    * Add syntax to integer DSL Instances
    */
  implicit class DSLIntOps[T[_], P](t: T[Int])(implicit D: DSL[T, P]) {
    def +(that: T[Int]): T[Int] = D.plus(t, that)

    def +(that: Int): T[Int] = D.plus(t, D.int(that))

    def -(that: T[Int]): T[Int] = D.minus(t, that)

    def -(that: Int): T[Int] = D.minus(t, D.int(that))

    def *(that: T[Int]): T[Int] = D.times(t, that)

    def *(that: Int): T[Int] = D.times(t, D.int(that))

    def <(that: T[Int]): T[Boolean] = D.lt(t, that)

    def <(that: Int): T[Boolean] = D.lt(t, D.int(that))

    def >(that: T[Int]): T[Boolean] = D.gt(t, that)

    def >(that: Int): T[Boolean] = D.gt(t, D.int(that))
  }

  /**
    * Lift an Int into the DSL domain.
    */
  implicit class DSLLiftOp(i: Int) {
    def lift[T[_]](implicit D: DSL[T, _]): T[Int] = D.int(i)
  }

}



