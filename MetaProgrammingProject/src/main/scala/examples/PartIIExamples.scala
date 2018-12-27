package examples

import core.user.dsl.{Empty, using}
import examples.part2.Objects.{Alice, Bob, Charlie, David}
import examples.part2.Schema
import examples.part2.Schema.Knows
import impl.memory.MemoryDB
import impl.memory.errors.MemoryError
import impl.part2.PartII

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object PartIIExamples {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  /**
    * Recreation of one of my original unit tests in my aprt II project.
    * (Name: Duplicates)
    *
    */
  def main(args: Array[String]): Unit = {
    val expectedDistinctPairs = Set(Alice -> Charlie)

    val expectedDistinctSingle = Set(Charlie)

    // open up a connection to a memory based database
    val db = MemoryDB.open(Empty, Schema.description).getOrElse(throw new Exception("Foo"))
    // wrap it a part III DSL
    val part3dsl = new PartII[MemoryError].toPartIII(db)
    // import the dsl to get access to all the syntax
    import part3dsl._

    // construct and run an operation
    /*
     * I didn't particularly understand the IO monad during my part II project, so running
     * an Operation returns a future that needs to be waited on.
     */

    concurrent.Await.result(using(db) {
      for {
        // set up the database with some simple relations
        _ <- inserts(
          (Alice, Knows, Bob), // there are two routes from Alice to Charlie
          (Bob, Knows, Charlie),
          (Alice, Knows, David),
          (David, Knows, Charlie)
        )

        // try reading from the database using readPair and readSingle
        res1 <- readPair(Knows.r -->--> Knows.r)

        /*
         * The lifting of values into the FindPair and FindSingle datatypes is a little clunky
         * as I don't like using invisible implicit conversions.
         */
        res3 <- readSingle(Alice.o >> Knows.r -->--> Knows.r)

        // print the results
        _ = println(res1)
              _ = println(res3)
            _ <- part3dsl.assertM(equalResult(expectedDistinctPairs, res1), "Distinct pairs failure")
            _ <- part3dsl.assertM(equalResult(expectedDistinctSingle, res3), "Distinct single failure")
      } yield ()
    }.run, 1000.seconds)
  }


}
