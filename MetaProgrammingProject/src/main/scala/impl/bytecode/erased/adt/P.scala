package impl.bytecode.erased.adt


/**
  * Type erased ADT for pair queries for compilation to bytecode.
  *
  * @tparam Procedure The type of callable procedures in the bytecode
  *                   compiler. This provides the procedure for generating
  *                   the correct identity procedure (which would otherwise
  *                   be lost by type erasure)
  *
  *                   Upto is intentionally not implemented as it can be freely implemented by exactly
  *
  */
sealed trait P[Procedure]

case class Prim[Procedure](p: Procedure) extends P[Procedure]

case class AndP[Procedure](p: P[Procedure], q: P[Procedure]) extends P[Procedure]

case class Distinct[Procedure](p: P[Procedure]) extends P[Procedure]

/**
  *
  * @param p - the identity procedure
  */
case class Id[Procedure](p: Procedure) extends P[Procedure]

/**
  * @param id - the identity procedure
  */
case class Exactly[Procedure](p: P[Procedure], n: Int, id: Procedure) extends P[Procedure]

case class OrP[Procedure](p: P[Procedure], q: P[Procedure]) extends P[Procedure]

case class Join[Procedure](p: P[Procedure], q: P[Procedure]) extends P[Procedure]

case class AndLeft[Procedure](p: P[Procedure], s: S[Procedure]) extends P[Procedure]

case class AndRight[Procedure](p: P[Procedure], s: S[Procedure]) extends P[Procedure]

case class Rev[Procedure](p: P[Procedure]) extends P[Procedure]

case class Fix[Procedure](p: P[Procedure], id: Procedure) extends P[Procedure]

