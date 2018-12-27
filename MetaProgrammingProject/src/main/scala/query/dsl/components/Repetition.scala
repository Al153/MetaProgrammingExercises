package query.dsl.components

/**
  * Objects for generating repetition queries
  */
sealed trait Repetition

case class Upto private(n: Int) extends Repetition

case class Between private(lo: Int, hi: Int) extends Repetition

case class AtLeast private(n: Int) extends Repetition
