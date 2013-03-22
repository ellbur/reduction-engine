
package reductionengine.logic

trait Node[+T]

case class App[+T](car: T, cdr: T) extends Node[T]

case class IntLiteral(n: Int) extends Node[Nothing]

case class Mystery(id: Int) extends Node[Nothing]

abstract class Operator(val name: String, val nArgs: Int)
case object Plus extends Operator("+", 2)
case object Times extends Operator("*", 2)
case object Minus extends Operator("-", 2)
case object S extends Operator("S", 3)
case object K extends Operator("K", 2)
case object I extends Operator("I", 1)
case object Y extends Operator("Y", 2)
case object B extends Operator("B", 2)

case class OperatorLiteral(operator: Operator) extends Node[Nothing]
