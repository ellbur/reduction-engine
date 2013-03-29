
package reductionengine.logic

trait Reductions { self: Nodes =>
  val nodeLike: NodeLike

  case class ReductionPossibility(name: String, remapping: RNode)

  sealed trait Replacement { def deepString: String }
  case class AlreadyThere(is: NodeType) extends Replacement {
    override def toString = "*"
    def deepString = is.toString
  }
  case class NewNode(is: Node) extends Replacement {
    override def toString = is.toString
    def deepString = is.deepString
  }

  trait NodeLike {
    def toNode(t: NodeType): Node
  }
  def toNode(x: RNode) = x match {
    case AlreadyThere(it) => nodeLike.toNode(it)
    case NewNode(it) => it
  }

  import self.{NewNode => NN}

  object AppLike {
    def unapply(x: RNode): Option[(RNode, RNode)] = toNode(x) match {
      case App(car, cdr) => Some((car, cdr))
      case _             => None
    }
  }

  object IntLiteralLike {
    def unapply(x: RNode) = toNode(x) match {
      case IntLiteral(n) => Some(n)
      case _ => None
    }
  }

  object OperatorLiteralLike {
    def unapply(x: RNode) = toNode(x) match {
      case OperatorLiteral(op) => Some(op)
      case _ => None
    }
  }

  object MysteryLike {
    def unapply(x: RNode) = toNode(x) match {
      case Mystery(n) => Some(Mystery(n))
      case _ => None
    }
  }

  object PureLike {
    def unapply(x: RNode) = toNode(x) match {
      case Pure(idiom, n) => Some((idiom, n))
      case _ => None
    }
  }

  object AntiPureLike {
    def unapply(x: RNode) = toNode(x) match {
      case AntiPure(idiom, n) => Some((idiom, n))
      case _ => None
    }
  }

  trait BuildWithNodes {
  }

  object StandardReductions {
    def find(x: RNode): Option[ReductionPossibility] = {
      import self.{NewNode => NN, AlreadyThere => AT}

      x match {
        case PureLike(idiom, expr) if toNode(expr).isPureIn(idiom) =>
          Some(ReductionPossibility(
            "Constify",
            NN(App(idiom.pure, expr))
          ))
        case PureLike(idiom, AppLike(a, b)) =>
          Some(ReductionPossibility(
            "Push Down",
            {
              val ma = NN(Pure(idiom, a))
              val mb = NN(Pure(idiom, b))
              NN(App(NN(App(idiom.app, ma)), mb))
            }
          ))
        case PureLike(i1, AntiPureLike(i2, x)) if i1 == i2 =>
          Some(ReductionPossibility(
            "Cancel",
            x
          ))
        case AntiPureLike(i1, PureLike(i2, x)) if i1 == i2 =>
          Some(ReductionPossibility(
            "Cancel",
            x
          ))
        case AppLike(
          OperatorLiteralLike(I),
          x
        ) =>
          Some(ReductionPossibility(
            "Reduce I",
            x
          ))
        case AppLike(AppLike(OperatorLiteralLike(K_), a), b) =>
          Some(ReductionPossibility(
            "Reduce K",
            a
          ))
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
        case AppLike(AppLike(AppLike(OperatorLiteralLike(S1), a), b), c) =>
          Some(ReductionPossibility(
            "Reduce S",
            NN(App(NN(App(a, c)), NN(App(b, c))))
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
}
