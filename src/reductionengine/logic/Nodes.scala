
package reductionengine.logic

trait Node[+T]

case class App[+T](car: T, cdr: T) extends Node[T]
object AppLike {
  def unapply[N<:NodeLike[N]](n: N): Option[(N, N)] = n.toNode match {
    case App(car, cdr) => Some((car, cdr))
    case _ => None
  }
}

case object Plus extends Node[Nothing]
object PlusLike {
  def unapply[N<:NodeLike[N]](n: N): Boolean = n.toNode match {
    case Plus => true
    case _ => false
  }
}

case class IntLiteral(n: Int) extends Node[Nothing]
object IntLiteralLike {
  def unapply[N<:NodeLike[N]](n: N): Option[Int] = n.toNode match {
    case IntLiteral(a) => Some(a)
    case _ => None
  }
}

trait NodeLike[N <: NodeLike[N]] {
  def toNode: Node[N]
}
