package part2.examples

import core.user.dsl.Relation
import core.user.schema._

object Schema {

  case class Person(n: String) {
    def name: String = n
  }

  implicit def personSchema = new SchemaObject1[Person, String] {
    override def construct(a1: String): Person = Person(a1)

    override def name: TableName = TableName("People")

    override def toTuple(a: Person): SchemaObject[Person] => DBTuple1[Person, String] = buildDBTuple(a.name)
  }

  case class Car(m: String) {
    def make: String = m
  }


  implicit def carSchema = new SchemaObject1[Car, String] {
    override def construct(a1: String): Car = Car(a1)

    override def name: TableName = TableName("Cars")

    override def toTuple(a: Car): SchemaObject[Car] => DBTuple1[Car, String] = buildDBTuple(a.make)
  }

  case class Pet(name: String, age: Int, height: Double, isDog: Boolean)

  implicit def petSchema = new SchemaObject4[Pet, String, Int, Double, Boolean] {
    override def construct(a1: String, a2: Int, a3: Double, a4: Boolean): Pet = Pet(a1, a2, a3, a4)

    override def name: TableName = TableName("Pets")

    override def toTuple(a: Pet): SchemaObject[Pet] => DBTuple4[Pet, String, Int, Double, Boolean] = buildDBTuple(a.name, a.age, a.height, a.isDog)
  }

  case object Knows extends Relation[Person, Person]

  case object Owns extends Relation[Person, Car]

  case object OwnedBy extends Relation[Pet, Person]


  implicit val description = new SchemaDescription(
    Set(personSchema, carSchema, petSchema),
    Set(Knows, Owns, OwnedBy)
  )

}