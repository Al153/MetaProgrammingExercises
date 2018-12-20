package query.dsl

import query.dsl.components._

import scala.language.higherKinds

/**
  * A standard implementation
  */
trait DSL[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], ToInsert[_, _], Valid[_]]
  extends Backend[M, Se, Pair, Single, Find, Path, ToInsert, Valid]
    with PairSyntaxProvider[Pair, Single, Valid]
    with SingleSyntaxProvider[Pair, Single, Find, Valid]
    with SymmetricSyntaxProvider[Pair, Single, Valid]
    with SimplePairs[Pair, Single, Valid]
    with SimpleRepetition[Pair, Valid]
    with FixedPoint[Pair, Valid]
    with SingleQueries[Pair, Single, Find, Valid]
    with BatchInserts[M, ToInsert, Valid]