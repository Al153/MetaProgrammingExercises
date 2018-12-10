package query.dsl.components

trait WithPairQueries[Pair[_, _], Single[_], Valid[_]]
  extends WithSimplePairs[Pair, Single, Valid]
  with WithSimpleRepetition[Pair, Valid]
  with WithFixedPoint[Pair, Valid]


trait WithSimplePairs[Pair[_, _], Single[_], Valid[_]] {
  def simplePairs: SimplePairs[Pair, Single, Valid]
}

trait WithSimpleRepetition[Pair[_, _], Valid[_]] {
  def simpleRepetition: SimpleRepetition[Pair, Valid]
}

trait WithFixedPoint[Pair[_, _], Valid[_]] {
  def fixedPoint: FixedPoint[Pair, Valid]
}