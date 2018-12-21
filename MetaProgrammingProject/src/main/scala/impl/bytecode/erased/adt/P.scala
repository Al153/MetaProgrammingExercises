package impl.bytecode.erased.adt

import impl.bytecode.Procedure

sealed trait P

case class Prim(p: Procedure) extends P

case class AndP(p: P, q: P) extends P

case class Distinct(p: P) extends P

case class Id(p: Procedure) extends P

case class Exactly(p: P, n: Int, id: Procedure) extends P

case class OrP(p: P, q: P) extends P

case class Join(p: P, q: P) extends P

case class AndLeft(p: P, s: S) extends P

case class AndRight(p: P, s: S) extends P

case class Rev(p: P) extends P

case class Fix(p: P, id: Procedure) extends P

