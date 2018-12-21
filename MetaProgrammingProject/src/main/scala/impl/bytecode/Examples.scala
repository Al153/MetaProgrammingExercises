package impl.bytecode

import query.dsl.components.UptoRange
import impl.trivial.Examples.{Car, Person}
import impl.trivial.Universe

object Examples extends ExampleObjects {

  import BytecodeCompiler._

  def main(args: Array[String]): Unit = {

    val joinTest = readPair(owns.p --><-- wants.p)
    println(joinTest.run())


    val repeatTest = readSingle(Alice.o >> (knows.p * UptoRange(4)))
    println(repeatTest.run())
  }

}

trait ExampleObjects {

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

  val small: Find[Car] = Set()
  val medium: Find[Car] = Set()
  val large: Find[Car] = Set()
  val volksWagens: Find[Car] = carUniverse.u.filter(_.brand == "Volkswagen")
  val fords: Find[Car] = carUniverse.u.filter(_.brand == "Ford")
}

