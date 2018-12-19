package query.dsl.components

/**
  * Simple monad typeclass
  * @tparam M - the monad
  */
trait Monad[M[_]] {
  def bind[A, B](ma: M[A], f: A => M[B]): M[B]

  def point[A](a: => A): M[A]
}

object Monad {

  /**
    * Some monadic syntax for integrating with scala's virtualised for-comprehensions
    */
  implicit class MonadOps[M[_] : Monad, A](ma: M[A]) {
    private val MM = implicitly[Monad[M]]

    def >>=[B](f: A => M[B]): M[B] = flatMap(f)

    def flatMap[B](f: A => M[B]): M[B] = MM.bind(ma, f)

    def map[B](f: A => B): M[B] = MM.bind[A, B](ma, a => MM.point[B](f(a)))
  }

}