package impl.part2.examples

import core.user.dsl.{Empty, _}
import impl.memory.MemoryDB
import impl.memory.errors.MemoryError
import impl.part2.PartII
import impl.part2.PartII._
import impl.part2.examples.Objects._
import impl.part2.examples.Schema._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object Examples {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  /**
    * Recreation of one of my original tests (Duplicates)
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    val expectedDistinctPairs = Set(Alice -> Charlie)

    val expectedDistinctSingle = Set(Charlie)

    val db = MemoryDB.open(Empty, Schema.description).getOrElse(throw new Exception("Foo"))
    val part3dsl = new PartII[MemoryError].toPartIII(db)
    import part3dsl._


    concurrent.Await.result(using(db) {
      for {
        _ <- inserts(
          (Alice, Knows, Bob), // there are two routes from Alice to Charlie
          (Bob, Knows, Charlie),
          (Alice, Knows, David),
          (David, Knows, Charlie)
        )

        res1 <- readPair(r(Knows) -->--> r(Knows))
        res3 <- readSingle(o(Alice) >> (r(Knows) -->--> r(Knows)))

        _ = println(res1)
        _ = println(res3)
        _ <- part3dsl.assertM(equalResult(expectedDistinctPairs, res1), "Distinct pairs failure")
        _ <- part3dsl.assertM(equalResult(expectedDistinctSingle, res3), "Distinct single failure")
      } yield ()
    }.run, 1000.seconds)
  }


}