package impl.bytecode.implementation.values

/**
  * Id of an object
  * @param universe the type ID
  * @param local the Local ID
  */
case class StackObject(universe: UniverseId, local: LocalId) {
  override def toString: String = s"StObj(U:${universe.id}; L:${local.id})"
}


