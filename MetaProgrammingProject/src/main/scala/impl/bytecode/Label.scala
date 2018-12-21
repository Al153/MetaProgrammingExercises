package impl.bytecode

case class Label(l: Int)

object Label {
  // yucky and impure :(
  private var count = 0

  def newLabel(): Label = {
    val l = Label(count)
    count += 1
    l
  }
}
