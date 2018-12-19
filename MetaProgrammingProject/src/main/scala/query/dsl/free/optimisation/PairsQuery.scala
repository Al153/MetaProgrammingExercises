package query.dsl.free.optimisation
import scala.language.higherKinds

// sealed trait OptimisedPairs[]
sealed trait PairsQuery[P[_, _], S[_],  A, B]
// case class p[P[_, _], A, B](p: P[A, B]) extends PairsQuery[P, S, A, B]


