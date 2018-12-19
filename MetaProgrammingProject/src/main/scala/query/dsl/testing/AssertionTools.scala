package query.dsl.testing

import query.dsl.components.{Backend, Monad}

import scala.language.higherKinds


trait TestSyntaxProvider[M[_], Se[_], Pair[_, _], Single[_], Path[_], Valid[_]] {
  self: TestTools[M, Se, Pair, Single, Path, Valid] =>

  implicit class PairTestSyntax[A: Valid, B: Valid](p: Pair[A, B]) {
    def =~=(q: Pair[A, B]): M[Boolean] = equalQuery(p, q)
  }

  implicit class SingleTestSyntax[A: Valid](s: Single[A]) {
    def =~=(t: Single[A]): M[Boolean] = equalQuery(s, t)
  }

  implicit class ResultSetSingleSyntax[A: Valid, B: Valid](r: Se[(A, B)]) {
    def ===(s: Se[(A, B)]): M[Boolean] = equalResult(r, s)
  }

  implicit class ResultSetPairSyntax[A: Valid](r: Se[A]) {
    def ===(s: Se[A]): M[Boolean] = equalResult(r, s)
  }

}

trait TestTools[M[_], Se[_], Pair[_, _], Single[_], Path[_], Valid[_]] {

  def equalQuery[A: Valid, B: Valid](p: Pair[A, B], q: Pair[A, B]): M[Boolean]

  def equalQuery[A: Valid](s: Single[A], q: Single[A]): M[Boolean]

  def equalResult[A: Valid](s: Se[A], t: Se[A]): M[Boolean]

  def equalResult[A: Valid, B: Valid](s: Se[(A, B)], t: Se[(A, B)]): M[Boolean]
}

trait AssertionTools[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], ToInsert[_, _], Valid[_]] {
  def assert(condition: M[Boolean]): M[Unit]
}

trait RunTimeTestTools[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], ToInsert[_, _], Valid[_]]
  extends TestTools[M, Se, Pair, Single, Path, Valid] {
  self: Backend[M, Se, Pair, Single, Find, Path, ToInsert, Valid] with Monad[M] =>

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
