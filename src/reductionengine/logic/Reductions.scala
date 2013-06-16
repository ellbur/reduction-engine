
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
    lazy val locallyReducible = toNode flatMap (_.locallyReducible)
    def isPureIn(idiom: Idiom) = toNode flatMap (_.isPureIn(idiom))
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
    lazy implicit val monad = self.monad
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

  val JLike = OperatorLiteralLike -/> {
    case J => ()
  }

  val PrLike = OperatorLiteralLike -/> {
    case Pr => ()
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
    case Pure(idiom, is) => (idiom, is)
  }

  val AntiPureLike = Id[RNode] --> (_.toNode) -/> {
    case AntiPure(idiom) => idiom
  }

  object StandardReductions {
    def find(x: RNode): M[Option[ReductionPossibility]] = {
      implicit val monad: Monad[M] = self.monad

      import self.{NewNode => NN, AlreadyThere => AT}

      val s = NN(OperatorLiteral(S1))
      val k = NN(OperatorLiteral(K_))
      val i = NN(OperatorLiteral(I))

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
        | AppLike(AppLike(JLike zip Id) zip Id) -> { case ((_, a), b) =>
            ReductionPossibility("Reduce J", a(b)(b))
          }
        | AppLike(AppLike(AppLike(PrLike zip Id) zip Id) zip Id) -> { case (((_, m), x), arg) =>
            ReductionPossibility("Reduce Pr",
              m
                (
                  s(i)(k(arg))
                )
                (x)
            )
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
        | PureLike --/> { case (idiom, x) =>
            x.isPureIn(idiom) map {
              case false => None
              case true =>
                val p = idiom.pure
                Some(ReductionPossibility("Constify",
                  p(x)
                ))
            }
          }
        // We limit push-down to places where it is not locally reducible in order to avoid
        // passing through an anti-pure.
        | PureLike(Id zip AppLike(Id zip Id)) --/> { case (idiom, (f, x)) =>
            f.locallyReducible flatMap {
              case true => None.pure[M]
              case false =>
                f mmatch (
                    AntiPureLike --> { otherIdiom =>
                      if (otherIdiom == idiom)
                        x.isPureIn(idiom) map {
                          case true =>
                            Some(ReductionPossibility("Cancel",
                              x
                            ))
                          case false =>
                            idiom.join match {
                              case None => None
                              case Some(join) =>
                                Some(ReductionPossibility("Join",
                                  join(NN(Pure(idiom, x)))
                                ))
                            }
                        }
                      else {
                        idiom.predict match {
                          case None =>
                            // We must pass through, hopefully leading up to a sequence.
                            val a = idiom.app
                            Some(ReductionPossibility("Push down",
                              a(NN(Pure(idiom, f)))(NN(Pure(idiom, x)))
                            )).pure[M]
                          case Some(predict) =>
                            val a = otherIdiom.app
                            val p = otherIdiom.pure
                            val map = s(k(a))(p)
                            Some(ReductionPossibility("Predict",
                              f(predict(map)(NN(Pure(idiom, x))))
                            )).pure[M]
                        }
                      }
                    }
                  | Id -> { _ =>
                      val a = idiom.app

                      Some(ReductionPossibility("Push down",
                        a(NN(Pure(idiom, f)))(NN(Pure(idiom, x)))
                      ))
                    }
                ) map (_.join)
            }
          }
      )
    }
  }

  def normalize(x: RNode): M[Option[RNode]] = {
    x.normalized
  }
}
