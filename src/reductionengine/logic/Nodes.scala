
package reductionengine.logic

import scalaz._
import Scalaz._
import setbounds._

trait Nodes { mod: Reductions =>
  // Agda parameterized module style.
  type M[+X]
  implicit val monad: Monad[M]

  type IdiomType
  type NodeType <: NodeLike

  type RNode = Replacement

  sealed trait ArgCount {
    def -(x: Int): ArgCount
    def full: Boolean
  }
  case class NArgs(n: Int) extends ArgCount {
    def -(x: Int) = NArgs(n - x)
    def full = n <= 0
  }
  case object NotAFunction extends ArgCount {
    def -(x: Int) = this
    def full = false
  }

  sealed trait Node {
    def isPureIn(idiom: Idiom) = upperImpurities map (! _.contains(idiom))
    def isNotImpureIn(idiom: Idiom) = lowerImpurities map (! _.contains(idiom))
    val upperImpurities: M[SetBound[Idiom]]
    val lowerImpurities: M[SetBound[Idiom]]
    def deepString: String
    /**
     * Assuming no reductions were possible here.
     * None indicates already normalized.
     */
    lazy val basicReductions = mod.basicReductions.find(NewNode(this))
    lazy val reductions = basicReductions flatMap {
      case None => mod.fancyReductions.find(NewNode(this))
      case Some(red) => Some(red).pure[M]
    }
    val normalized: M[Option[RNode]]
    lazy val normalizedOrSame = normalized map (_ getOrElse NewNode(this))
    val fullyApplied: M[Boolean]
    override def toString = deepString
  }

  case class Idiom(rep: IdiomType, pure: RNode, app: RNode, join: Option[RNode], predict: Option[RNode]) {
    override def toString = rep.toString
  }

  case class App(car: RNode, cdr: RNode) extends Node {
    lazy val upperImpurities = (car.upperImpurities |@| cdr.upperImpurities)(_ ++ _)
    lazy val lowerImpurities = (car.lowerImpurities |@| cdr.lowerImpurities)(_ ++ _)
    override def toString = s"$car($cdr)"
    def deepString = s"${car.deepString}(${cdr.deepString})"
    lazy val normalized: M[Option[RNode]] =
      reductions flatMap {
        case Some(red @ ReductionPossibility(_, remapping)) =>
          println("Reducing")
          some(remapping.normalized map (_ getOrElse remapping)).sequence
        case None =>
          normalizedNoReductions
      }
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
    lazy val fullyApplied = car.toNode flatMap (_ match {
      case Pure(_) => true.pure[M]
      case _ =>
        basicReductions flatMap (_.isDefined match {
          case true => true.pure[M]
          case false => car.fullyApplied flatMap {
            case true => true.pure[M]
            case false => cdr.fullyApplied
          }
        })
    })
  }
  case class Pure(of: Idiom) extends Node {
    lazy val upperImpurities = SetBound[Idiom]().pure[M]
    lazy val lowerImpurities = SetBound[Idiom]().pure[M]
    def deepString = s"Pure(${of.toString})"
    lazy val normalized: M[Option[RNode]] = None.pure[M]
    val fullyApplied = false.pure[M]
  }
  case class AntiPure(of: Idiom) extends Node {
    lazy val upperImpurities = SetBound(of).pure[M]
    lazy val lowerImpurities = upperImpurities
    def deepString = s"ak(${of.toString})"
    val normalized = None.pure[M]
    val fullyApplied = false.pure[M]
  }

  case class IntLiteral(n: Int) extends Node {
    lazy val upperImpurities = SetBound[Idiom]().pure[M]
    lazy val lowerImpurities = SetBound[Idiom]().pure[M]
    override def toString = n.toString
    def deepString = n.toString
    lazy val normalized = None.pure[M]
    val fullyApplied = false.pure[M]
  }

  case class Mystery(id: String) extends Node {
    lazy val lowerImpurities = Lower[Idiom](Set()).pure[M]
    lazy val upperImpurities = Upper[Idiom](Set()).pure[M]
    override def toString = "?"
    def deepString = "?"
    lazy val normalized = None.pure[M]
    val fullyApplied = true.pure[M]
  }

  case class OperatorLiteral(operator: Operator) extends Node {
    lazy val upperImpurities = SetBound[Idiom]().pure[M]
    lazy val lowerImpurities = SetBound[Idiom]().pure[M]
    def deepString = operator.toString
    lazy val normalized = None.pure[M]
    val fullyApplied = false.pure[M]
  }

  import NameUtils._

  abstract class Operator(val name: String, val nArgs: Int) {
    val nTrueArgs: ArgCount = NArgs(nArgs)
  }
  case object Plus extends Operator("+", 2)
  case object Times extends Operator("*", 2)
  case object Minus extends Operator("-", 2)
  case class S(order: Int) extends Operator(s"S$order", 3)
  val S1 = S(1)
  case class K(select: List[Boolean]) extends Operator(kName(select),
    if (select.length > 0) 2 else 1)
  val K_ = K(List(false))
  val I = K(Nil)
  case object Y extends Operator("Y", 2)
  case object B extends Operator("B", 2)
  case object J extends Operator("J", 2)
  case object Pr extends Operator("Pr", 3)
  case object PairListNil extends Operator("[]", 0) {
    override val nTrueArgs = NotAFunction
  }
  case object PairListCons extends Operator("::", 2) {
    override val nTrueArgs = NotAFunction
  }
  case object MaybeNothing extends Operator("Nothing", 0) {
    override val nTrueArgs = NotAFunction
  }
  case object MaybeJust extends Operator("Just", 1) {
    override val nTrueArgs = NotAFunction
  }
  case object Eliminator extends Operator("match", 3)

  object NameUtils {
    def kName(select: Seq[Boolean]): String =
      if (select.length == 0)
        "I"
      else
        "K" ++ (select map { s =>
          if (s) "x" else "_"
        } mkString "")
  }

  def mergeKBits(b1: List[Boolean], b2: List[Boolean]): List[Boolean] = b1 match {
    case Nil => b2
    case true :: b1Rest => b2 match {
      case Nil => true :: b1Rest
      case true :: b2Rest => true :: mergeKBits(b1Rest, b2Rest)
      case false :: b2Rest => false :: mergeKBits(b1Rest, b2Rest)
    }
    case false :: b1Rest => false :: mergeKBits(b1Rest, b2)
  }
}
