package query.dsl.testing

import query.TestTools
import query.dsl.components.{Backend, HasMonad}

import scala.language.higherKinds


/**
  * Freely implement query equality by running both queries and comparing th results.
  */
trait RuntimeTestTools[M[_], Se[_], Pair[_, _], Single[_], Find[_], Path[_], ToInsert[_, _], Valid[_]]
  extends TestTools[M, Se, Pair, Single, Valid] {
  self: Backend[M, Se, Pair, Single, Find, Path, ToInsert, Valid] with HasMonad[M] =>

  import query.dsl.components.Monad._


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
