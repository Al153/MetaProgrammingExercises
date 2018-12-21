package impl.bytecode.erased.adt

import impl.bytecode.Procedure

/**
  * Type-Erased ADT for Single Queries
  */
sealed trait S

case class PrimS(p: Procedure) extends S

case class OrS(s: S, t: S) extends S

case class AndS(s: S, t: S) extends S

case class From(s: S, p: P) extends S
