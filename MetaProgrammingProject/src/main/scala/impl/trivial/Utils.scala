package impl.trivial

import scala.collection.mutable

object Utils {
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
