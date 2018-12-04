package query.dsl.components

trait WithSingleQueries[Pair[_, _], Single[_], Find[_], Valid[_]] {
  def singleQueries: SingleQueries[Pair, Single, Find, Valid]
}
