package query.dsl.components

import scala.language.higherKinds

/**
  * Simple mix in trait to provide batch insert syntax
  */
trait BatchInserts[M[_], ToInsert[_, _], Valid[_]] {
  self: Writes[M, ToInsert, Valid] =>
  final def inserts[A: Valid, B: Valid](xs: ToInsert[A, B]*): M[Unit] = self.insert(xs.seq)
}
