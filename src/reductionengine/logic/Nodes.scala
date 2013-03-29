
package reductionengine.logic

trait Nodes { this: Reductions =>
  // Agda parameterized module style.
  type IdiomType
  type NodeType

  type RNode = Replacement

  sealed trait Node {
    def isPureIn(idiom: Idiom): Boolean
    def deepString: String
  }

  case class Idiom(rep: IdiomType, pure: RNode, app: RNode) {
    override def toString = rep.toString
  }

  case class App(car: RNode, cdr: RNode) extends Node {
    def isPureIn(idiom: Idiom) = toNode(car).isPureIn(idiom) && toNode(cdr).isPureIn(idiom)
    override def toString = s"$car($cdr)"
    def deepString = s"${car.deepString}(${cdr.deepString})"
  }
  case class Pure(of: Idiom, is: RNode) extends Node {
    def isPureIn(idiom: Idiom) = false // TODO
    def deepString = s"Pure(${of.toString}, ${is.deepString})"
  }
  case class AntiPure(of: Idiom, is: RNode) extends Node {
    def isPureIn(idiom: Idiom) = false // TODO
    def deepString = s"AntiPure(${of.toString}, ${is.deepString})"
  }

  case class IntLiteral(n: Int) extends Node {
    def isPureIn(idiom: Idiom) = true
    override def toString = n.toString
    def deepString = n.toString
  }

  case class Mystery(id: Int) extends Node {
    def isPureIn(idiom: Idiom) = false
    override def toString = "?"
    def deepString = "?"
  }

  case class OperatorLiteral(operator: Operator) extends Node {
    def isPureIn(idiom: Idiom) = true
    def deepString = operator.toString
  }

  import NameUtils._

  abstract class Operator(val name: String, val nArgs: Int)
  case object Plus extends Operator("+", 2)
  case object Times extends Operator("*", 2)
  case object Minus extends Operator("-", 2)
  case class S(order: Int) extends Operator(s"S$order", 3)
  val S1 = S(1)
  case class K(order: Int, select: Seq[Boolean]) extends Operator(kName(select), 2)
  val K_ = K(1, Seq(false))
  case object I extends Operator("I", 1)
  case object Y extends Operator("Y", 2)
  case object B extends Operator("B", 2)
  case object J extends Operator("J", 1)
  case object E extends Operator("E", 1)

  object NameUtils {
    def kName(select: Seq[Boolean]): String =
      "K" ++ (select map { s =>
        if (s) "x" else "_"
      } mkString "")
  }
}
