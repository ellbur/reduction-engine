
package reductionengine.logic

trait Node[+T]

case class Idiom(name: String, pure: Replacement[Nothing], app: Replacement[Nothing]) {
  override def toString = name
}

case class App[+T](idiomStack: List[Idiom], car: T, cdr: T) extends Node[T] {
  override def toString = s"$car($cdr)"
}
case class Pure[+T](of: Idiom, ignoring: List[Idiom], is: T) extends Node[T]
case class AntiPure[+T](of: Idiom, ignoring: List[Idiom], is: T) extends Node[T]

case class IntLiteral(n: Int) extends Node[Nothing] {
  override def toString = n.toString
}

case class Mystery(id: Int) extends Node[Nothing] {
  override def toString = "?"
}

import NameUtils._

abstract class Operator(val name: String, val nArgs: Int)
case object Plus extends Operator("+", 2)
case object Times extends Operator("*", 2)
case object Minus extends Operator("-", 2)
case class S(order: Int) extends Operator(s"S$order", 3)
case class K(order: Int, select: Seq[Boolean]) extends Operator(kName(select), 2)
case object I extends Operator("I", 1)
case object Y extends Operator("Y", 2)
case object B extends Operator("B", 2)
case object J extends Operator("J", 1)
case object E extends Operator("E", 1)

case class OperatorLiteral(operator: Operator) extends Node[Nothing]

object NameUtils {
  def kName(select: Seq[Boolean]): String =
    "K" ++ (select map { s =>
      if (s) "x" else "_"
    } mkString "")
}
