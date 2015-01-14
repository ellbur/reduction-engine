
package setbounds

sealed trait SetBound[A] {
  def contains(x: A): Boolean
  def -(x: A): SetBound[A]

  def ++(other: SetBound[A]) = (this, other) match {
    case (Lower(c1), Lower(c2)) => Lower(c1 ++ c2)
    case (Upper(nc), Lower(c)) => Upper(nc -- c)
    case (Lower(c), Upper(nc)) => Upper(nc -- c)
    case (Upper(nc1), Upper(nc2)) => Upper(nc1 intersect nc2)
  }
}

object SetBound {
  def apply[A](x: A*) = Lower[A](x.toSet)
}

case class Lower[A](containing: Set[A]) extends SetBound[A] {
  def contains(x: A) = containing.contains(x)
  def -(x: A) = Lower(containing - x)
}

case class Upper[A](notContaining: Set[A]) extends SetBound[A] {
  def contains(x: A) = !notContaining.contains(x)
  def -(x: A) = Upper(notContaining + x)
}
