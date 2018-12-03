package query.dsl

trait WithPairQueries[Pair[_, _], Single[_], Valid[_]] {
  def pairQueries: PairQueries[Pair, Single, Valid]
}
