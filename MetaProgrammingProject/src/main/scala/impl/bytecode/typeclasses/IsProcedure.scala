package impl.bytecode.typeclasses

import scala.language.higherKinds

/**
  * Type class ensuring we can create procedures from compilable values and relations.
  * @tparam P - The procedure class
  * @tparam Relation - the relation class
  * @tparam Compilable - compilable typeclass
  * @tparam Find - findable type.
  */
abstract class IsProcedure[P, Relation[_, _], Compilable[_], Find[_]] {
  def convertRelation[A: Compilable, B: Compilable](r: Relation[A, B]): P

  def convertObject[A: Compilable](a: A): P

  def convertFindable[A: Compilable](fa: Find[A]): P
}
