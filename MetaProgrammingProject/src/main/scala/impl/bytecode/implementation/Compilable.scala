package impl.bytecode.implementation

import impl.bytecode.Find
import impl.bytecode.implementation.values._

import scala.collection.mutable

class Compilable[A] private(val typeId: UniverseId) {
  private var localIdCount: Long = 0

  def extract(r: StackValue): Set[A] =
    r match {
      case SingleRelation(objs) => objs map recoveryMap.apply
    }

  def id: Procedure = Procedure(s => PairRelation(objects.toSet.map((a: StackObject) => a -> a)) :: s)

  def toProc(f: Find[A]): Procedure = {
    val set: Set[StackObject] = for (a <- f) yield {
      convert(a)
    }
    Procedure(s => SingleRelation(set) :: s)
  }

  def toProc(a: A): Procedure = {
    Procedure(s => SingleRelation(Set(convert(a))) :: s)
  }

  def convert(a: A): StackObject = if (universe.contains(a)) {
    conversionMap(a)
  } else {

    val obj = StackObject(typeId, LocalId(localIdCount))

    universe += a
    objects += obj
    recoveryMap += obj -> a
    conversionMap += a -> obj
    localIdCount += 1
    obj
  }

  val universe: mutable.Set[A] = new mutable.HashSet[A]()

  val objects: mutable.Set[StackObject] = new mutable.HashSet[StackObject]()

  def extract(stackObject: StackObject): A = recoveryMap(stackObject)


  private val conversionMap: mutable.Map[A, StackObject] = new mutable.HashMap[A, StackObject]()
  private val recoveryMap: mutable.Map[StackObject, A] = new mutable.HashMap[StackObject, A]()
}

object Compilable {
  def create[A](): Compilable[A] = new Compilable[A](newUniverseId())

  private var count: Int = 0

  // Note: should be synchronised in the case of being in
  private def newUniverseId(): UniverseId = {
    val res = UniverseId(count)
    count += 1
    res
  }
}
