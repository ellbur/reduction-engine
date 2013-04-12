
package reductionengine.logic

trait Nodes { this: Reductions =>
  // Agda parameterized module style.
  type IdiomType
  type NodeType <: NodeLike

  type RNode = Replacement

  sealed trait Node {
    def isPureIn(idiom: Idiom): Boolean
    def isNotImpureIn(idiom: Idiom): Boolean
    def deepString: String
    /**
     * Assuming no reductions were possible here.
     * None indicates already normalized.
     */
    lazy val normalized: Option[RNode] =
      StandardReductions.find(NewNode(this)) match {
        case Some(ReductionPossibility(_, remapping)) =>
          Some(remapping.normalized getOrElse remapping)
        case None =>
          normalizedNoReductions
      }
    val normalizedNoReductions: Option[RNode]
    lazy val normalizedOrSame = normalized getOrElse (NewNode(this))
  }

  case class Idiom(rep: IdiomType, pure: RNode, app: RNode) {
    override def toString = rep.toString
  }

  case class App(car: RNode, cdr: RNode) extends Node {
    def isPureIn(idiom: Idiom) = car.toNode.isPureIn(idiom) && cdr.toNode.isPureIn(idiom)
    def isNotImpureIn(idiom: Idiom) = car.toNode.isNotImpureIn(idiom) && cdr.toNode.isNotImpureIn(idiom)
    override def toString = s"$car($cdr)"
    def deepString = s"${car.deepString}(${cdr.deepString})"
    lazy val normalizedNoReductions =
      car.normalized match {
        case Some(better) =>
          Some(App(better, cdr).normalizedOrSame)
        case None =>
          cdr.normalized match {
            case Some(better) =>
              Some(App(car, better).normalizedOrSame)
            case None =>
              None
          }
      }
  }
  case class Pure(of: Idiom, is: RNode) extends Node {
    def isPureIn(idiom: Idiom) =
      if (idiom == of)
        true
      else
        is.toNode.isPureIn(idiom)
    def isNotImpureIn(idiom: Idiom) =
      if (idiom == of)
        true
      else
        is.toNode.isNotImpureIn(idiom)
    def deepString = s"Pure(${of.toString}, ${is.deepString})"
    lazy val normalizedNoReductions =
      is.normalized match {
        case None => None
        case Some(better) => Some(Pure(of, better).normalizedOrSame)
      }
  }
  case class AntiPure(of: Idiom, is: RNode) extends Node {
    def isPureIn(idiom: Idiom) =
      if (idiom == of)
        false
      else
        is.toNode.isPureIn(idiom)
    def isNotImpureIn(idiom: Idiom) =
      if (idiom == of)
        false
      else
        is.toNode.isNotImpureIn(idiom)
    def deepString = s"AntiPure(${of.toString}, ${is.deepString})"
    lazy val normalizedNoReductions =
      is.normalized match {
        case None => None
        case Some(better) => Some(AntiPure(of, better).normalizedOrSame)
      }
  }

  case class IntLiteral(n: Int) extends Node {
    def isPureIn(idiom: Idiom) = true
    def isNotImpureIn(idiom: Idiom) = true
    override def toString = n.toString
    def deepString = n.toString
    lazy val normalizedNoReductions = None
  }

  case class Mystery(id: Int) extends Node {
    def isPureIn(idiom: Idiom) = false
    def isNotImpureIn(idiom: Idiom) = true
    override def toString = "?"
    def deepString = "?"
    lazy val normalizedNoReductions = None
  }

  case class OperatorLiteral(operator: Operator) extends Node {
    def isPureIn(idiom: Idiom) = true
    def isNotImpureIn(idiom: Idiom) = true
    def deepString = operator.toString
    lazy val normalizedNoReductions = None
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
