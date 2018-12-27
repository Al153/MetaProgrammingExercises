package examples

import impl.trivial._
import query.dsl.components.AtLeast

/**
  * Some simple examples of running the TrivialBackend
  */
object TrivialExamples extends Objects {

  import TrivialBackend._

  def main(args: Array[String]): Unit = {
    // Intellij/Scala can infer the type (the type is automatically annotated in Intellij
    // :trivial.Relation[Person, Person]
    def envies = wants --><-- owns
    // :trivial.Relation[Person, Person]
    def ownSame = owns --><-- owns
    // :trivial.Relation[Person, Person]
    def wantSame = wants --><-- wants

    // we can simply execute the queries
    println("Simple query: " + envies)
    // or use the more backend-independent syntax
    println("Using readPair (redundant): " + readPair(envies * AtLeast(1)))

    // Queries are associative
    println(wants --><-- wants -->--> owns)

    // syntax allows for us to join over particular sets
    println("Nobody envies someone over a small car: "  + (wants --> small <-- owns))

    // pathfinding over relations
    println("All Shortest Paths: " + allShortestPaths(Alice, envies | ownSame | wantSame))

  }


}

case class Car(brand: String, model: String)

case class Person(name: String)


trait Objects {
  val ModelT = Car("Ford", "Model T")
  val Focus = Car("Ford", "Focus")
  val Fiesta = Car("Ford", "Fiesta")
  val Mondeo = Car("Ford", "Mondeo")
  val Golf = Car("Volkswagen", "Golf")
  val Polo = Car("Volkswagen", "Polo")
  val Beatle = Car("Volkswagen", "Beatle")
  val Passat = Car("Volkswagen", "Passat")
  val Alice = Person("Alice")

  implicit val carUniverse: Universe[Car] = new Universe[Car](Set(
    ModelT,
    Focus,
    Fiesta,
    Mondeo,
    Golf,
    Polo,
    Beatle,
    Passat
  ))
  val Bob = Person("Bob")
  val Charlie = Person("Charlie")
  val Dave = Person("Dave")
  val Eve = Person("Eve")
  val Fred = Person("Fred")
  val George = Person("George")
  val Hanna = Person("Hanna")
  val Ian = Person("Ian")
  val Jane = Person("Jane")
  val Katherine = Person("Katherine")
  val Lucy = Person("Lucy")
  val Mallory = Person("Mallory")
  val Nat = Person("Nat")
  val Oliver = Person("Oliver")
  val Peter = Person("Peter")
  val owns: Relation[Person, Car] = Set(
    Alice -> ModelT,
    Bob -> Fiesta,
    Bob -> Passat,
    Charlie -> Beatle,
    Dave -> Polo,
    Dave -> Golf
  )
  val wants: Relation[Person, Car] = Set(
    Bob -> Mondeo,
    Bob -> ModelT,
    Charlie -> Golf,
    Alice -> Beatle,
    Dave -> Mondeo
  )


  implicit val personUniverse: Universe[Person] = new Universe[Person](
    Set(
      Alice,
      Bob,
      Charlie,
      Dave,
    )
  )
  val small: Set[Car] = Set()
  val medium: Set[Car] = Set()
  val large: Set[Car] = Set()
  val volksWagens: Set[Car] = carUniverse.u.filter(_.brand == "Volkswagen")
  val fords: Set[Car] = carUniverse.u.filter(_.brand == "Ford")

}