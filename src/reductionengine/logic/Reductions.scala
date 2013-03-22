
package reductionengine.logic

case class ReductionPossibility[R](name: String, remapping: Replacement[R])

sealed trait Replacement[+N]

case class AlreadyThere[+N](is: N) extends Replacement[N]
case class NewNode[+N](is: Node[Replacement[N]]) extends Replacement[N]

trait NodeLike[S] {
  def toNode(t: Replacement[S]): Node[Replacement[S]]
}
object NodeLike {
  def toNode[T:NodeLike](x: Replacement[T]) = implicitly[NodeLike[T]].toNode(x)
}
import NodeLike.toNode

import reductionengine.logic.{NewNode => NN}

object AppLike {
  def unapply[T:NodeLike](x: Replacement[T]) = toNode(x) match {
    case App(car, cdr) => Some((car, cdr))
    case _ => None
  }
}

object IntLiteralLike {
  def unapply[T:NodeLike](x: Replacement[T]) = toNode(x) match {
    case IntLiteral(n) => Some(n)
    case _ => None
  }
}

object OperatorLiteralLike {
  def unapply[T:NodeLike](x: Replacement[T]) = toNode(x) match {
    case OperatorLiteral(op) => Some(op)
    case _ => None
  }
}

trait BuildWithNodes {
}

object StandardReductions {
  def find[S:NodeLike](x: Replacement[S]): Option[ReductionPossibility[S]] = {
    import reductionengine.logic.{NewNode => NN, AlreadyThere => AT}

    x match {
      case AppLike(AppLike(OperatorLiteralLike(Plus), IntLiteralLike(a)), IntLiteralLike(b)) =>
        Some(ReductionPossibility(
          "Perform addition",
          NN(IntLiteral(a + b))
        ))
      case AppLike(AppLike(OperatorLiteralLike(Minus), IntLiteralLike(a)), IntLiteralLike(b)) =>
        Some(ReductionPossibility(
          "Perform subtraction",
          NN(IntLiteral(a - b))
        ))
      case AppLike(AppLike(OperatorLiteralLike(Times), IntLiteralLike(a)), IntLiteralLike(b)) =>
        Some(ReductionPossibility(
          "Perform multiplication",
          NN(IntLiteral(a * b))
        ))
      case AppLike(AppLike(AppLike(OperatorLiteralLike(S), a), b), c) =>
        Some(ReductionPossibility(
          "Reduce S",
          NN(App(NN(App(a, c)), NN(App(b, c))))
        ))
      case AppLike(AppLike(OperatorLiteralLike(K), a), b) =>
        Some(ReductionPossibility(
          "Reduce K",
          a
        ))
      case AppLike(OperatorLiteralLike(I), a) =>
        Some(ReductionPossibility(
          "Reduce I",
          a
        ))
      case AppLike(AppLike(y @ OperatorLiteralLike(Y), f), x) =>
        Some(ReductionPossibility(
          "Reduce Y",
          NN(App(
            NN(App(
              f,
              NN(App(y, f))
            )),
            x
          ))
        ))
      case _ => None
    }
  }
}
