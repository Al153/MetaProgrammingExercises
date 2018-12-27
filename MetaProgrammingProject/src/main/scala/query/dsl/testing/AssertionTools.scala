package query.dsl.testing

import scala.language.higherKinds


/**
  * Make an assertion on a boolean
  *
  * @tparam M - monad
  */
trait AssertionTools[M[_]] {
  def assertM(condition: M[Boolean], msg: String): M[Unit]
}
