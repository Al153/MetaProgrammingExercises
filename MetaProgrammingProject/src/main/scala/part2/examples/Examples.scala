package part2.examples

import concurrent.duration._
import core.user.dsl.{Empty, _}
import impl.memory.MemoryDB
import impl.memory.errors.MemoryError
import part2.PartII
import part2.PartII._
import part2.examples.Objects._
import part2.examples.Schema._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object Examples {
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  def main(args: Array[String]): Unit = {
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
        //_ <- assertEqOp(expectedDistinctPairs, res1, "Distinct pairs failure")
        //_ <- assertEqOp(expectedDistinctSingle, res3, "Distinct single failure")
      } yield ()
    }.run, 1000.seconds)
  }


}
