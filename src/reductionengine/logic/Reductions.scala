
package reductionengine.logic

import scalaz._
import Scalaz._
import ellbur.monadicmatch.MonadicMatch

trait Reductions { self: Nodes =>
  case class ReductionPossibility(name: String, remapping: RNode)

  sealed trait Replacement {
    def deepString: String
    val toNode: M[Node]
    lazy val normalized: M[Option[RNode]] = toNode flatMap (_.normalized)

    def apply(x: RNode): RNode = NewNode(App(this, x))
  }
  case class AlreadyThere(is: NodeType) extends Replacement {
    override def toString = "*"
    def deepString = is.toString
    lazy val toNode = is.toNode
  }
  case class NewNode(is: Node) extends Replacement {
    override def toString = is.toString
    def deepString = is.deepString
    lazy val toNode = is.pure[M]
  }

  trait NodeLike {
    val toNode: M[Node]
  }

  import self.{NewNode => NN}

  object monadicMatch extends MonadicMatch {
    type M[+X] = self.M[X]
    lazy val monad = self.monad
  }
  import monadicMatch._

  val AppLike = Id[RNode] --> (_.toNode) -/> {
    case App(car, cdr) => (car, cdr)
  }

  val IntLiteralLike = Id[RNode] --> (_.toNode) -/> {
    case IntLiteral(n) => n
  }

  val OperatorLiteralLike = Id[RNode] --> (_.toNode) -/> {
    case OperatorLiteral(op) => op
  }

  val SLike = OperatorLiteralLike -/> {
    case S(n) => n
  }

  val KLike = OperatorLiteralLike -/> {
    case K(n, ss) => (n, ss)
  }

  val ILike = OperatorLiteralLike -/> {
    case I => ()
  }

  val YLike = OperatorLiteralLike -/> {
    case Y => ()
  }

  val PlusLike = OperatorLiteralLike -/> {
    case Plus => ()
  }

  val TimesLike = OperatorLiteralLike -/> {
    case Times => ()
  }

  val MinusLike = OperatorLiteralLike -/> {
    case Minus => ()
  }

  val MysteryLike = Id[RNode] --> (_.toNode) -/> {
    case Mystery(n) => Mystery(n)
  }

  val PureLike = Id[RNode] --> (_.toNode) -/> {
    case Pure(idiom, n) => (idiom, n)
  }

  val AntiPureLike = Id[RNode] --> (_.toNode) -/> {
    case AntiPure(idiom, n) => (idiom, n)
  }

  object StandardReductions {
    def find(x: RNode): M[Option[ReductionPossibility]] = {
      import self.{NewNode => NN, AlreadyThere => AT}

      x mmatch (
          AppLike(ILike zip Id) -> { case (_, x) =>
            ReductionPossibility("Reduce I", x)
          }
        | AppLike(AppLike(AppLike(SLike zip Id) zip Id) zip Id) -> { case (((order, a), b), c) =>
            ReductionPossibility("Reduce S", order match {
              case 0 => a(b)(c)
              case 1 => a(c)(b(c))
              case n =>
                val Sm = NN(OperatorLiteral(S(n - 1)))
                Sm(a(c))(b(c))
            })
          }
        | AppLike(AppLike(KLike zip Id) zip Id) -> { case (((order, ss), a), b) =>
            ReductionPossibility("Reduce K", order match {
              case 0 => a(b)
              case 1 =>
                if (ss(0)) a(b)
                else a
              case n =>
                val Km = NN(OperatorLiteral(K(n-1, ss.tail)))
                if (ss.head) {
                  Km(a(b))
                }
                else {
                  Km(a)
                }
            })
          }
        | AppLike(AppLike(YLike zip Id) zip Id) -> { case ((_, f), x) =>
            val y = NN(OperatorLiteral(Y))
            ReductionPossibility("Reduce Y", f(y(f))(x))
          }
        | AppLike(AppLike(PlusLike zip IntLiteralLike) zip IntLiteralLike) -> { case ((_, a), b) =>
            ReductionPossibility("Perform Addition", NN(IntLiteral(a + b)))
          }
        | AppLike(AppLike(TimesLike zip IntLiteralLike) zip IntLiteralLike) -> { case ((_, a), b)
            => ReductionPossibility("Perform Multiplication", NN(IntLiteral(a * b)))
          }
        | AppLike(AppLike(MinusLike zip IntLiteralLike) zip IntLiteralLike) -> { case ((_, a), b)
            => ReductionPossibility("Perform Subtraction", NN(IntLiteral(a - b)))
          }
      )
    }
  }

  def normalize(x: RNode): M[Option[RNode]] = {
    x.normalized
  }
}
