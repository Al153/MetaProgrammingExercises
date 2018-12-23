package impl.bytecode.implementation

import impl.bytecode.implementation.values._

import scala.collection.mutable

/**
  * The compilable type-class for types.
  * Consists of maps for reading and writing values
  * of type A to stack values and vice versa.
  *
  * @param typeId - The universe Id for the type (not really needed if we assume that the compilation is sound)
  * @tparam A - the type to map to and from.
  */
final class Compilable[A] private(val typeId: UniverseId) {
  // used to assign each object a unique ID
  private var localIdCount: Long = 0

  /**
    * Universe of already seen values (have existing ID)
    */
  private val universe: mutable.Set[A] = new mutable.HashSet[A]()

  /**
    * Set of stack objects assigned a value.
    * (under this implementation is a bit redundant
    * (the set is the set of values less than the counter))
    */
  private val objects: mutable.Set[StackObject] = new mutable.HashSet[StackObject]()

  /**
    * The insertion and extraction maps
    */
  private val conversionMap: mutable.Map[A, StackObject] = new mutable.HashMap[A, StackObject]()
  private val recoveryMap: mutable.Map[StackObject, A] = new mutable.HashMap[StackObject, A]()


  /**
    * extract an actual value from the stack object
    * @param stackObject
    * @return
    */
  def extract(stackObject: StackObject): A = recoveryMap(stackObject)

  /**
    * Extract a set of values
    */
  def extract(r: StackValue): Set[A] =
    r match {
      case SingleRelation(objs) => objs map recoveryMap.apply
    }

  /**
    * Procedure for an ID query.
    */
  def id: Procedure = Procedure(s => PairRelation(objects.toSet.map((a: StackObject) => a -> a)) :: s)


  /**
    * Converts a findable to a procedure.
    * @param f
    * @return
    */
  def toProc(f: Find[A]): Procedure = {
    val set: Set[StackObject] = for (a <- f) yield {
      convert(a)
    }
    Procedure(s => SingleRelation(set) :: s)
  }

  /**
    * Converts a single object to a procedure.
    */
  def toProc(a: A): Procedure = {
    Procedure(s => SingleRelation(Set(convert(a))) :: s)
  }

  /**
    * Convert an object to a stack object, creating a record if necessary.
    */
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

}

object Compilable {
  /**
    * createa new [[Compilable]] instance.
    */
  def create[A](): Compilable[A] = new Compilable[A](newUniverseId())

  private var count: Int = 0

  // Note: should be synchronised in the case of being in a concurrent system/
  private def newUniverseId(): UniverseId = {
    val res = UniverseId(count)
    count += 1
    res
  }
}
