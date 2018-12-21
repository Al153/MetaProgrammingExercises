package impl.bytecode.erased.adt

sealed trait P[+Procedure]

case class Prim[Procedure](p: Procedure) extends P[Procedure]

case class AndP[Procedure](p: P[Procedure], q: P[Procedure]) extends P[Procedure]

case class Distinct[Procedure](p: P[Procedure]) extends P[Procedure]

case class Id[Procedure](p: Procedure) extends P[Procedure]

case class Exactly[Procedure](p: P[Procedure], n: Int, id: Procedure) extends P[Procedure]

case class OrP[Procedure](p: P[Procedure], q: P[Procedure]) extends P[Procedure]

case class Join[Procedure](p: P[Procedure], q: P[Procedure]) extends P[Procedure]

case class AndLeft[Procedure](p: P[Procedure], s: S[Procedure]) extends P[Procedure]

case class AndRight[Procedure](p: P[Procedure], s: S[Procedure]) extends P[Procedure]

case class Rev[Procedure](p: P[Procedure]) extends P[Procedure]

case class Fix[Procedure](p: P[Procedure], id: Procedure) extends P[Procedure]

