package part2

import core.backend.intermediate.{FindPair, Rel}
import core.user.dsl.{CompletedRelation, Empty, Relation}
import core.user.schema._
import impl.memory.MemoryDB
import impl.memory.errors.MemoryError
import part2.Schema.{Car, Person, Pet}

import scala.concurrent.ExecutionContext.Implicits.global

object Examples {

  def main(args: Array[String]): Unit = {
    val part3dsl = new PartII[MemoryError].toPartIII(MemoryDB.open(Empty, ???).getOrElse(throw new Exception("Foo")))

    import Objects._
    import Schema._
    import part3dsl._


    def insertBatch[A: SchemaObject, B: SchemaObject](xs: CompletedRelation[A, B]*) = insert(xs.seq)

    for {

      _ <- insertBatch(
        CompletedRelation(Alice, Knows, Bob), // there are two routes from Alice to Charlie
        CompletedRelation(Bob, Knows, Charlie),
        CompletedRelation(Alice, Knows, David),
        CompletedRelation(David, Knows, Charlie)
      )

      res1 <- readPair(Knows -->--> Knows)
      res3 <- find(Alice >> (Knows -->--> Knows))

      _ <- assertEqOp(expectedDistinctPairs, res1, "Distinct pairs failure")
      _ <- assertEqOp(expectedDistinctSingle, res3, "Distinct single failure")
    } yield ()


  }
}

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

object Objects {
  val Alice = Person("Alice")
  val Bob = Person("Bob")
  val Charlie = Person("Charlie")
  val David = Person("David")
  val Eve = Person("Eve")
  val Fred = Person("Fred")
  val Georgie = Person("Georgie")
  val Hannah = Person("Hannah")
  val Ian = Person("Ian")
  val Jane = Person("Jane")

  // to be used to test failure cases
  val Zoe = Person("Zoe")

  val Mercedes = Car("Mercedes")
  val Ford = Car("Ford")
  val Bentley = Car("Bentley")
  val VW = Car("VW")

  // Pets
  val fido = Pet("Fido", 1, 60.5, isDog = true)
  val rover = Pet("Rover", 2, 49.5, isDog = true)
  val polly = Pet("Polly", 3, 25.4, isDog = false)
  val leo = Pet("Leo", 18, 29, isDog = false)
  val buster = Pet("Buster", 13, 20, isDog = true)
  val gus = Pet("Gus", 4, 30, isDog = false)
  val fin = Pet("Fin", 4, 28, isDog = false)
  val tufty = Pet("Tufty", 6, 6, isDog = false)
  val tilly = Pet("Tilly", 5, 6, isDog = false)
  val pippa = Pet("Pippa", 12, 45, isDog = true)
  val luna = Pet("Luna", 6, 24.5, isDog = false)
  val nelson = Pet("Nelson", 3, 60.1, isDog = true)
  val lucy = Pet("Lucy", 3, 20, isDog = true)
  val jasper = Pet("Jasper", 12, 45, isDog = true)
}