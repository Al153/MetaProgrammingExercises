package query.dsl

import query.dsl.components._

trait DSL[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]]
  extends Backend[M, Se, Pair, Single, Find, Path, R, Valid]
    with Monad[M]
    with PairSyntaxProvider[M, Se, Pair, Single, Find, Path, R, Valid]
    with SingleSyntaxProvider[M, Se, Pair, Single, Find, Path, R, Valid]
    with WithPairQueries[Pair, Single, Valid]
    with WithSingleQueries[Pair, Single, Find, Valid]
