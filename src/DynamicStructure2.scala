
object DynamicStructure2 extends scala.App {
  /*
  trait Node[Provider[_]]

  trait AppProvider[P] { def car: ProvidedNode[P] ; def cdr: ProvidedNode[P] }
  case object App extends Node[AppProvider]

  case class ProvidedNode[P](node: Node[P], provision: P)
  */

  /*
  sealed trait ReductionNode[B]
  case class AlreadyThere[B](bubble: B) extends ReductionNode[B]
  case class NewNode[B](node: ProvidedNode[ReductionNode[B]]) extends ReductionNode[B]
  */

  // Let's start by doing this the non-typeclass-haskell way.
  // Usually the safest.

  /*
  trait NodeStyle {
    type ProvisionType[T]

    def unapply[T](b: Bubble[T]): Option[ProvisionType[T]] = b.unapply(this)
  }

  case object App extends NodeStyle {
    type ProvisionType[T] = (T, T)
  }

  case object IntLiteral extends NodeStyle {
    type ProvisionType[T] = Int
  }

  trait Bubble[T <: Bubble[T]] { // lol bubble tea.
    def unapply(nodeStyle: NodeStyle): Option[nodeStyle.ProvisionType[T]]
  }

  def magic23[T](b: Bubble[T]) = b match {
    case App(IntLiteral(2), IntLiteral(3)) => true
    case _ => false
  }

  trait StraightUpBubble extends Bubble[StraightUpBubble]

  case class StraightUpApp(car: StraightUpBubble, cdr: StraightUpBubble) extends StraightUpBubble {
    def unapply(nodeStyle: NodeStyle) = nodeStyle match {
      case a: App => Some((car, cdr))
      case _ => None
    }
  }

  case class StraightUpIntLiteral(n: Int) extends StraightUpBubble {
    def unapply(nodeStyle: NodeStyle) = nodeStyle match {
      case IntLiteral => Some(n)
      case _ => None
    }
  }

  val formula1 = StraightUpApp(StraightUpIntLiteral(2), StraightUpIntLiteral(3))
  val formula2 = StraightUpApp(StraightUpIntLiteral(2), StraightUpIntLiteral(4))

  println(magic23(formula1))
  println(magic23(formula2))
  */
}
