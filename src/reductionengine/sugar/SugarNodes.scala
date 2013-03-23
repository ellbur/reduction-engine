
package reductionengine.sugar

import reductionengine.logic
import logic.{NewNode => NN, AlreadyThere => AT, Node, Replacement, NodeLike, NodeUtils}

trait SugarNode[+T] {
  def toNode: logic.Node[logic.Replacement[T]]
  val children: Traversable[T]
}
object SugarNode {
  def translateDeep[T:logic.NodeLike](n: logic.Replacement[T]): SugarReplacement[T] = {
    import SugarNode.{translateDeep => t}

    n match {
      case logic.AlreadyThere(it) => AlreadyThere(it)
      case logic.NewNode(n) => NewNode(n match {
        case logic.App(idioms, car, cdr) =>
          ??? // TODO
        case logic.IntLiteral(n) =>
          IntLiteral(n)
        case logic.OperatorLiteral(op) =>
          ApicalOperator(BasicOperator(op), Seq())
        case logic.Mystery(n) =>
          Mystery(n)
        case _ =>
          Mystery(0)
      })
    }
  }
}

case class IntLiteral(n: Int) extends SugarNode[Nothing] {
  def toNode = logic.IntLiteral(n)
  val children = Seq()
}

sealed abstract class SugarOperator(val name: String, val nArgs: Int)
case class BasicOperator(op: logic.Operator) extends SugarOperator(op.name, op.nArgs)
case object B extends SugarOperator("B", 2)

case class ApicalOperator[+N](op: SugarOperator, args: Seq[N])
  extends SugarNode[N]
{
  def toNode = {
    op match {
      case B =>
        NodeUtils.applyAll(args) match {
          case logic.NewNode(it) => it
          case it @ logic.AlreadyThere(_) =>
            logic.App(List(), NN(logic.OperatorLiteral(logic.I)), it)
        }
      case BasicOperator(op) =>
        NodeUtils.applyAllTo(logic.OperatorLiteral(op), args)
    }
  }
  val children = args

  override def toString = op match {
    case B =>
      args.toList match {
        case Nil => "I"
        case List(x) => x.toString
        case first :: rest => s"$first(${rest map (_.toString) mkString ", "})"
      }
    case op =>
      s"$op(${args map (_.toString) mkString ", "}})"
  }
}

object App {
  def apply[T](car: T, cdr: T): ApicalOperator[T] =
    ApicalOperator(B, Seq(car, cdr))
}

/**
 * The goal is  that eventually this will be a replacement for ApicalOperator.
 */
case class IdiomaticJunction[+N](op: SugarOperator, args: IdiomaticReconciliation[N]) extends SugarNode[N] {
  def toNode = ???
  lazy val children = ???
}

case class Root[+N](is: N)(implicit nl: logic.NodeLike[N]) extends SugarNode[N] {
  def toNode = nl.toNode(AT(is))
  lazy val children = Seq(is)
}

case class Mystery(id: Int) extends SugarNode[Nothing] {
  def toNode = logic.Mystery(id)
  val children = Seq()
}

case class Open(id: String) extends SugarNode[Nothing] {
  def toNode = logic.Mystery(0)
  val children = Seq()
  override def toString = id
}

case class NumberEditor(id: Int, progress: String) extends SugarNode[Nothing] {
  def toNode = logic.Mystery(id)
  val children = Seq()
}

case class Focused[+N](is: SugarNode[N]) extends SugarNode[N] {
  def toNode = is.toNode
  lazy val children = is.children
}

sealed trait SugarReplacement[+T] {
  val height: Int
  def apply[S >: T](cdr: SugarReplacement[S]) = NewNode(App(this, cdr))
}
case class AlreadyThere[+T](t: T) extends SugarReplacement[T] {
  val height = 0
  override def toString = "*"
}
case class NewNode[+T](s: SugarNode[SugarReplacement[T]]) extends SugarReplacement[T] {
  lazy val height = {
    val ch = s.children map (_.height)
    if (ch.isEmpty)
      0
    else
      1 + ch.max
  }
  override def toString = s.toString
}
