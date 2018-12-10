package trivial

import TrivialBackend._
import query.dsl.components.AtleastRange

// checking branch
object Examples {

  case class Car(brand: String, model: String)

  val ModelT = Car("Ford", "Model T")
  val Focus = Car("Ford", "Focus")
  val Fiesta = Car("Ford", "Fiesta")
  val Mondeo = Car("Ford", "Mondeo")
  val Golf = Car("Volkswagen", "Golf")
  val Polo = Car("Volkswagen", "Polo")
  val Beatle = Car("Volkswagen", "Beatle")
  val Passat = Car("Volkswagen", "Passat")

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

  case class Person(name: String)

  val Alice = Person("Alice")
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


  implicit val personUniverse: Universe[Person] = new Universe[Person](
    Set(
      Alice,
      Bob,
      Charlie,
      Dave,
    )
  )

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

  val small: Set[Car] = Set()
  val medium: Set[Car] = Set()
  val large: Set[Car] = Set()

  val volksWagens: Set[Car] = carUniverse.u.filter(_.brand == "Volkswagen")
  val fords: Set[Car] = carUniverse.u.filter(_.brand == "Ford")

  // Intellij/Scala can infer the type
  def envies = wants --><-- owns

  def main(args: Array[String]): Unit = {
    println(envies)

    println(envies * AtleastRange(1))
  }
}
