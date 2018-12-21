package impl.bytecode.erased.adt

import impl.bytecode.implementation.Procedure

/**
  * Type-Erased ADT for Single Queries
  */
sealed trait S[Procedure]

case class PrimS[Procedure](p: Procedure) extends S[Procedure]

case class OrS[Procedure](s: S[Procedure], t: S[Procedure]) extends S[Procedure]

case class AndS[Procedure](s: S[Procedure], t: S[Procedure]) extends S[Procedure]

case class From[Procedure](s: S[Procedure], p: P[Procedure]) extends S[Procedure]
