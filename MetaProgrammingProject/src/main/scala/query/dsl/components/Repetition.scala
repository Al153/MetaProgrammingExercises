package query.dsl.components

sealed trait Repetition

case class UptoRange private(n: Int) extends Repetition

case class BetweenRange private(lo: Int, hi: Int) extends Repetition

case class AtleastRange private(n: Int) extends Repetition
