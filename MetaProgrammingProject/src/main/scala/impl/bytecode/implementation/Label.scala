package impl.bytecode.implementation

/**
  * A label is simply a unique id
  */
case class Label(l: Int)

object Label {
  // yucky and impure :(
  private var count = 0

  /**
    * Generate a new label
   */
  def newLabel(): Label = {
    val l = Label(count)
    count += 1
    l
  }
}
