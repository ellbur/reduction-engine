
package reductionengine.logic

import scalaz._
import Scalaz._

trait Nodes { this: Reductions =>
  // Agda parameterized module style.
  type M[+X]
  implicit val monad: Monad[M]

  type IdiomType
  type NodeType <: NodeLike

  type RNode = Replacement

  sealed trait Node {
    def isPureIn(idiom: Idiom): M[Boolean]
    def isNotImpureIn(idiom: Idiom): M[Boolean]
    def deepString: String
    /**
     * Assuming no reductions were possible here.
     * None indicates already normalized.
     */
    lazy val normalized: M[Option[RNode]] =
      StandardReductions.find(NewNode(this)) flatMap {
        case Some(ReductionPossibility(_, remapping)) =>
          some(remapping.normalized map (_ getOrElse remapping)).sequence
        case None =>
          normalizedNoReductions
      }
    val normalizedNoReductions: M[Option[RNode]]
    lazy val normalizedOrSame = normalized map (_ getOrElse (NewNode(this)))
  }

  case class Idiom(rep: IdiomType, pure: RNode, app: RNode) {
    override def toString = rep.toString
  }

  case class App(car: RNode, cdr: RNode) extends Node {
    def isPureIn(idiom: Idiom) =
      car.toNode flatMap { car =>
        car.isPureIn(idiom) flatMap { carPure =>
          if (!carPure)
            false.pure
          else
            cdr.toNode flatMap { cdr =>
              cdr.isPureIn(idiom)
            }
        }
      }
    def isNotImpureIn(idiom: Idiom) =
      car.toNode flatMap { car =>
        car.isNotImpureIn(idiom) flatMap { carNotImpure =>
          if (!carNotImpure)
            false.pure
          else
            cdr.toNode flatMap { cdr =>
              cdr.isNotImpureIn(idiom)
            }
        }
      }
    override def toString = s"$car($cdr)"
    def deepString = s"${car.deepString}(${cdr.deepString})"
    lazy val normalizedNoReductions =
      car.normalized flatMap {
        case Some(better) =>
          some(App(better, cdr).normalizedOrSame).sequence
        case None =>
          cdr.normalized flatMap {
            case Some(better) =>
              some(App(car, better).normalizedOrSame).sequence
            case None =>
              None.pure
          }
      }
  }
  case class Pure(of: Idiom, is: RNode) extends Node {
    def isPureIn(idiom: Idiom) =
      if (idiom == of)
        true.pure
      else
        is.toNode flatMap (_.isPureIn(idiom))
    def isNotImpureIn(idiom: Idiom) =
      if (idiom == of)
        true.pure
      else
        is.toNode flatMap (_.isNotImpureIn(idiom))
    def deepString = s"Pure(${of.toString}, ${is.deepString})"
    lazy val normalizedNoReductions =
      is.normalized flatMap {
        case None => None.pure
        case Some(better) => some(Pure(of, better).normalizedOrSame).sequence
      }
  }
  case class AntiPure(of: Idiom, is: RNode) extends Node {
    def isPureIn(idiom: Idiom) =
      if (idiom == of)
        false.pure
      else
        is.toNode flatMap (_.isPureIn(idiom))
    def isNotImpureIn(idiom: Idiom) =
      if (idiom == of)
        false.pure
      else
        is.toNode flatMap (_.isNotImpureIn(idiom))
    def deepString = s"AntiPure(${of.toString}, ${is.deepString})"
    lazy val normalizedNoReductions =
      is.normalized flatMap {
        case None => None.pure
        case Some(better) => some(AntiPure(of, better).normalizedOrSame).sequence
      }
  }

  case class IntLiteral(n: Int) extends Node {
    def isPureIn(idiom: Idiom) = true.pure[M]
    def isNotImpureIn(idiom: Idiom) = true.pure[M]
    override def toString = n.toString
    def deepString = n.toString
    lazy val normalizedNoReductions = None.pure[M]
  }

  case class Mystery(id: String) extends Node {
    def isPureIn(idiom: Idiom) = false.pure[M]
    def isNotImpureIn(idiom: Idiom) = true.pure[M]
    override def toString = "?"
    def deepString = "?"
    lazy val normalizedNoReductions = None.pure[M]
  }

  case class OperatorLiteral(operator: Operator) extends Node {
    def isPureIn(idiom: Idiom) = true.pure[M]
    def isNotImpureIn(idiom: Idiom) = true.pure[M]
    def deepString = operator.toString
    lazy val normalizedNoReductions = None.pure[M]
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
