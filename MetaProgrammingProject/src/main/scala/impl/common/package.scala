package impl

import query.dsl.components.Monad

import scala.collection.mutable

package object common {
  type Id[A] = A

  implicit object IdMonad extends Monad[Id] {
    override def bind[A, B](ma: Id[A], f: A => Id[B]): Id[B] = f(ma)

    override def point[A](a: => A): Id[A] = a
  }

  def joinSet[A, B, C](leftRes: Set[(A, B)], rightRes: Set[(B, C)]): Set[(A, C)] = {
    // build an index of all values to join on right, since Proj_B(right) is a subset of Proj_B(Left)
    val collectedRight = mutable.Map[B, mutable.Set[C]]()
    for ((b, c) <- rightRes) {
      val s = collectedRight.getOrElseUpdate(b, mutable.Set())
      s += c
    }

    for {
      (left, middle) <- leftRes
      right <- collectedRight.getOrElse(middle, Set[C]())
    } yield (left, right)
  }
}
