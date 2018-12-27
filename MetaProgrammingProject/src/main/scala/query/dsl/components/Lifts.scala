package query.dsl.components

import scala.language.higherKinds


/**
  * Allows lifting of separate relation and object types into queries
  * @tparam Relation - Host language/front end relation type
  * @tparam Pair - DSL pair relation type
  * @tparam Single - DSL single relation type
  * @tparam Valid - type validity type-class
  */
trait Lifts[Relation[_, _], Pair[_, _], Single[_], Valid[_]] {
  implicit class RelationSyntax[A: Valid, B: Valid](rel: Relation[A, B]) {
    def r: Pair[A, B] = relationToPair(rel)
  }

  implicit class ObjectSyntax[A: Valid](a: A) {
    def o: Single[A] = objectToSingle(a)
  }

  protected def relationToPair[A: Valid, B: Valid](relation: Relation[A, B]): Pair[A, B]

  protected def objectToSingle[A: Valid](a: A): Single[A]

}
