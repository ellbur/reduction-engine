
object GraphLayout {
  sealed trait LayoutNode
  case class Fixed(x: Double, y: Double) extends LayoutNode
  case object Free extends LayoutNode

  trait ToLayoutNode {
    def toLayoutNode: LayoutNode
  }

  def computeLayout[T <: ToLayoutNode](
    nodes: Traversable[T], edges: Traversable[(T, T)])
  {

  }
}
