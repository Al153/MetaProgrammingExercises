package examples

import impl.bytecode.implementation._
import impl.trivial.Universe
import query.dsl.components.Upto

object BytecodeExamples extends ExampleObjects {
  val impl = new SimpleImplementation.Implementation()
  import impl._

  def main(args: Array[String]): Unit = {

    // Test a simple join query
    val joinTest = readPair(owns.r --><-- wants.r)
    println("Joins: " + joinTest.run().toList.sorted)


    // test an upto query (tests the bytecode loop unrollingloop function)
    val repeatTest = readSingle(Alice.o >> (knows.r * Upto(4)))
    println("Short Loops: " + repeatTest.run().toList.sorted)

    // test the operation of non-unrolled looping
    val longRepeatsTest = readSingle(Alice.o >> (knows.r * Upto(10)))
    println("Long Loops: " + longRepeatsTest.run().toList.sorted)

    // Test exactly query
    val exactlyTest = readPair((knows.r -->--> knows.r) * 2)
    println("Exactly: " + exactlyTest.run().toList.sorted)

    // Test the bytecode fixed point loop.
    val fixedPointTest = readSingle(Alice.o >> fixedPoint(knows.r))
    println("Fixed Point: " + fixedPointTest.run().toList.sorted)

    def envies = wants.r --><-- owns.r


    // testing And and Or
    val andTest = readPair(fixedPoint(knows.r) & envies)
    println("And Test: " + andTest.run().toList.sorted)

    val orTest = readPair(knows.r | (knows.r -->--> knows.r))
    println("Or Test: " + orTest.run().toList.sorted)

    // AndS, OrS
    println("AndS: " + readSingle(small.f & volksWagens.f).run().toList.sorted)
    println("OrS: " + readSingle(small.f | volksWagens.f).run().toList.sorted)

    // From
    println("From: " + readSingle(small.f >> owns.r.rev).run().toList.sorted)



    // testing AndLeft and AndRight
    println("AndLeft" + readPair(small.f ->>- owns.r.rev).run().toList.sorted)
    println("AndRight" + readPair(owns.r ->>- small.f).run().toList.sorted)


  }

}

trait ExampleObjects {
  implicit def PersonOrdering: Ordering[Person] = Ordering.by(_.name)

  implicit def CarOrdering: Ordering[Car] = Ordering.Tuple2[String, String].on(c => (c.brand, c.model))

  implicit val PersonCompilable: Compilable[Person] = Compilable.create[Person]()

  implicit val CarCompilable: Compilable[Car] = Compilable.create[Car]()


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
  /** Knows Matrix
    *
    * A -> B -> C
    * |    |    |
    * \/   \/   \/
    * D -> E -> F
    * |    |    |
    * \/   \/   \/
    * G -> H -> I
    */

  val knows: Relation[Person, Person] = new Relation[Person, Person] {
    override def pairs: Set[(Person, Person)] =
      Set(
        Alice -> Bob,
        Bob -> Charlie,
        Alice -> Dave,
        Bob -> Eve,
        Charlie -> Fred,
        Dave -> Eve,
        Eve -> Fred,
        Dave -> George,
        Eve -> Hanna,
        Fred -> Ian,
        George -> Hanna,
        Hanna -> Ian

      )
  }

  val owns: Relation[Person, Car] = new Relation[Person, Car] {
    override def pairs: Set[(Person, Car)] = Set(
      Alice -> ModelT,
      Bob -> Fiesta,
      Bob -> Passat,
      Charlie -> Beatle,
      Dave -> Polo,
      Dave -> Golf
    )
  }

  val wants: Relation[Person, Car] = new Relation[Person, Car] {
    override def pairs = Set(
      Bob -> Mondeo,
      Bob -> ModelT,
      Charlie -> Golf,
      Alice -> Beatle,
      Dave -> Mondeo
    )
  }

  val small: Find[Car] = Set(Fiesta, Polo, Beatle)
  val medium: Find[Car] = Set(Focus, Golf)
  val large: Find[Car] = Set(Mondeo, Passat)
  val volksWagens: Find[Car] = carUniverse.u.filter(_.brand == "Volkswagen")
  val fords: Find[Car] = carUniverse.u.filter(_.brand == "Ford")
}

