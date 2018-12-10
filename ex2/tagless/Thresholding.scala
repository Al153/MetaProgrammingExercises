package tagless

import tagless.TaglessFinal.DSL
import scala.language.higherKinds

trait Thresholding[Wrapper[_], Picture] {
  self: DSL[Wrapper, Picture] =>

  /**
    * Max of two values
    */
  def max(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]

  /**
    * Min of two values
    */
  def min(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]
}

/**
  * A "Free" implementation of max and min.
  *
  * DSL writers may choose to provide a faster custom implementation.
  */
object DefaultThresholdImplementation {
  implicit def Default[T[_], P](implicit D: DSL[T, P]): DSL[T, P] with Thresholding[T, P] =

    new DSL[T, P] with Thresholding[T, P] {
      override def if_[A](c: T[Boolean], t: T[A], e: T[A]): T[A] = D.if_(c, t, e)

      /**
        * Run a program on a picture
        */
      override def run(program: T[Int], picture: P): P = D.run(program, picture)

      /**
        * Max of two values
        */
      override def max(a: T[Int], b: T[Int]): T[Int] = if_(gt(a, b), a, b)

      /**
        * Min of two values
        */
      override def min(a: T[Int], b: T[Int]): T[Int] = if_(lt(a, b), a, b)

      /**
        * Puts an int
        */
      override def int(i: Int): T[Int] = D.int(i)

      /**
        * Puts the original value
        *
        * @return
        */
      override def it: T[Int] = D.it

      /**
        * Add two values
        */
      override def plus(a: T[Int], b: T[Int]): T[Int] = D.plus(a, b)

      /**
        * Subtract right value from left value
        */
      override def minus(a: T[Int], b: T[Int]): T[Int] = D.minus(a, b)

      /**
        * Multiply two values
        */
      override def times(a: T[Int], b: T[Int]): T[Int] = D.times(a, b)

      /**
        * Compare two values
        */
      override def lt(a: T[Int], b: T[Int]): T[Boolean] = D.lt(a, b)

      /**
        * Compare two values
        */
      override def gt(a: T[Int], b: T[Int]): T[Boolean] = D.gt(a, b)

      /**
        * Branch according to a condition
        */

    }
}
