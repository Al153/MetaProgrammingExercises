package query.dsl.testing

import query.dsl.components.{Backend, Monad}

trait WithTestTools[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]] {
  def testTools: TestTools[M, Se, Pair, Single, Find, Path, R, Valid]
}

trait TestSyntaxProvider[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]] {
  self: WithTestTools[M, Se, Pair, Single, Find, Path, R, Valid] =>

  implicit class PairTestSyntax[A: Valid, B: Valid](p: Pair[A, B]) {
    def =~=(q: Pair[A, B]): M[Boolean] = testTools.equalQuery(p, q)
  }

  implicit class SingleTestSyntax[A: Valid](s: Single[A]) {
    def =~=(t: Single[A]): M[Boolean] = testTools.equalQuery(s, t)
  }

  implicit class ResultSetSingleSyntax[A: Valid, B: Valid](r: Se[(A, B)]) {
    def ===(s: Se[(A, B)]): M[Boolean] = testTools.equalResult(r, s)
  }

  implicit class ResultSetPairSyntax[A: Valid](r: Se[A]) {
    def ===(s: Se[A]): M[Boolean] = testTools.equalResult(r, s)
  }

}

trait TestTools[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]] {

  def equalQuery[A: Valid, B: Valid](p: Pair[A, B], q: Pair[A, B]): M[Boolean]

  def equalQuery[A: Valid](s: Single[A], q: Single[A]): M[Boolean]

  def equalResult[A: Valid](s: Se[A], t: Se[A]): M[Boolean]

  def equalResult[A: Valid, B: Valid](s: Se[(A, B)], t: Se[(A, B)]): M[Boolean]
}

trait AssertionTools[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]] {
  def assert(condition: M[Boolean]): M[Unit]
}

trait RunTimeTestTools[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], R[_, _], Valid[_]]
  extends TestTools[M, Se, Pair, Single, Find, Path, R, Valid] {
  self: Backend[M, Se, Pair, Single, Find, Path, R, Valid] with Monad[M] =>

  import query.dsl.components.Monad._
  implicit val MonadM: Monad[M] = this

  override def equalQuery[A: Valid, B: Valid](p: Pair[A, B], q: Pair[A, B]): M[Boolean] =
    for {
      pRes <- readPair(p)
      qRes <- readPair(q)
      equal <- equalResult(pRes, qRes)
    } yield equal

  override def equalQuery[A: Valid](s: Single[A], t: Single[A]): M[Boolean] =
    for {
      sRes <- readSingle(s)
      tRes <- readSingle(t)
      equal <- equalResult(sRes, tRes)
    } yield equal
}
