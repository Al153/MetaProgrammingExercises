package impl.bytecode.erased.adt

/**
  * Type-Erased ADT for Single Queries
  *
  * @tparam Procedure Provides the procedure type for the underlying bytecode implementatin
  *                   This is to allow compilation of find terms and identity terms.
  */
sealed trait S[Procedure]

case class PrimS[Procedure](p: Procedure) extends S[Procedure]

case class OrS[Procedure](s: S[Procedure], t: S[Procedure]) extends S[Procedure]

case class AndS[Procedure](s: S[Procedure], t: S[Procedure]) extends S[Procedure]

case class From[Procedure](s: S[Procedure], p: P[Procedure]) extends S[Procedure]
