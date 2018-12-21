package impl.bytecode.values

case class StackObject(universe: UniverseId, local: LocalId) {
  override def toString: String = s"StObj(U:${universe.id}; L:${local.id})"
}


