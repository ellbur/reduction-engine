
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
    lazy val normalizedOrSame = normalized map (_ getOrElse this)
    def apply(x: RNode): NewNode = NewNode(App(this, x))
    def isPureIn(idiom: Idiom) = toNode flatMap (_.isPureIn(idiom))
    lazy val upperImpurities = toNode flatMap (_.upperImpurities)
    lazy val lowerImpurities = toNode flatMap (_.lowerImpurities)
    lazy val fullyApplied: M[Boolean] = toNode flatMap (_.fullyApplied)
    lazy val reductions = toNode flatMap (_.reductions)
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
    case K(ss) => ss
  }

  val KNotILike = OperatorLiteralLike -/> {
    case K(ss) if ss.length > 0 => ss
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
    case Pure(idiom) => idiom
  }

  val AntiPureLike = Id[RNode] --> (_.toNode) -/> {
    case AntiPure(idiom) => idiom
  }

  val PairListNilLike = OperatorLiteralLike -/> {
    case PairListNil => ()
  }

  val PairListConsLike = OperatorLiteralLike -/> {
    case PairListCons => ()
  }

  val MaybeNothingLike = OperatorLiteralLike -/> {
    case MaybeNothing => ()
  }

  val MaybeJustLike = OperatorLiteralLike -/> {
    case MaybeJust => ()
  }

  val EliminatorLike = OperatorLiteralLike -/> {
    case Eliminator => ()
  }

  val LambdaLike: Extractor[Idiom,Idiom] = Id[Idiom] --/> { case idiom =>
    idiom.app mmatch (
      (SLike when (_ == 1)) -> { _ => idiom }
    )
  }

  object basicReductions {
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
        | AppLike(AppLike(KLike zip Id) zip Id) -> { case ((select, a), b) =>
            ReductionPossibility("Reduce K", select match {
              case Nil => a(b)
              case true :: Nil => a(b)
              case false :: Nil => a
              case head :: rest =>
                val Km = NN(OperatorLiteral(K(rest)))
                if (head) {
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
        | AppLike(AppLike(TimesLike zip IntLiteralLike) zip IntLiteralLike) -> { case ((_, a), b) =>
            ReductionPossibility("Perform Multiplication", NN(IntLiteral(a * b)))
          }
        | AppLike(AppLike(MinusLike zip IntLiteralLike) zip IntLiteralLike) -> { case ((_, a), b) =>
            ReductionPossibility("Perform Subtraction", NN(IntLiteral(a - b)))
          }
        | AppLike(AppLike(AppLike(EliminatorLike zip PairListNilLike) zip Id) zip Id) -> { case (((_, _), f), g) =>
            ReductionPossibility("Match []", f)
          }
        | AppLike(AppLike(AppLike(EliminatorLike zip AppLike(AppLike(PairListConsLike zip Id) zip Id)) zip Id) zip Id) -> {
            case (((_, ((_, car), cdr)), f), g) =>
              ReductionPossibility("Match ::", g(car)(cdr))
          }
        | AppLike(AppLike(AppLike(EliminatorLike zip MaybeNothingLike) zip Id) zip Id) -> { case (((_, _), f), g) =>
            ReductionPossibility("Match Nothing", f)
          }
        | AppLike(AppLike(AppLike(EliminatorLike zip AppLike(MaybeJustLike zip Id)) zip Id) zip Id) -> { case (((_, (_, x)), f), g) =>
            ReductionPossibility("Match Just", g(x))
          }
      )
    }
  }

  object fancyReductions {
    def find(x: RNode): M[Option[ReductionPossibility]] = {
      implicit val monad: Monad[M] = self.monad

      import self.{NewNode => NN, AlreadyThere => AT}

      val s = NN(OperatorLiteral(S1))
      val k = NN(OperatorLiteral(K_))
      val i = NN(OperatorLiteral(I))

      x mmatch (
          AppLike(AppLike(SLike zip AppLike(KLike zip Id)) zip ILike) -/> { case ((1, (List(false), f)), _) =>
            ReductionPossibility("η-Contract",
              f
            )
          }
        | AppLike(AppLike(SLike zip AppLike(KLike zip Id)) zip AppLike(KLike zip ILike)) -/> {
            case ((n, (b1, f)), (b2, i)) if (n>1) && (b1==(List.fill(n-1)(true) ++ List(false))) && (b2==List.fill(n-1)(false)) =>
              ReductionPossibility(s"η-Contract ($n, $b1, $b2, $f, $i)",
                f
              )
          }
        | AppLike(KLike zip AppLike(KLike zip Id)) -> { case (s1, (s2, a)) =>
            ReductionPossibility("Merge K",
              NN(OperatorLiteral(K(mergeKBits(s1, s2))))(a)
            )
          }
        | AppLike(PureLike zip Id) --/> { case (idiom, x) =>
            x.isPureIn(idiom) map {
              case false => None
              case true =>
                val p = idiom.pure
                Some(ReductionPossibility("Constify",
                  p(x)
                ))
            }
          }
        | AppLike(PureLike(LambdaLike) zip AppLike(AppLike(SLike zip Id) zip Id)) -> { case (idiom, ((n, a), b)) =>
            val sNext = NN(OperatorLiteral(S(n + 1)))
            ReductionPossibility("Push down",
              sNext(NN(Pure(idiom))(a))(NN(Pure(idiom))(b))
            )
          }
        | AppLike(PureLike(LambdaLike) zip AppLike(KNotILike zip Id)) -> { case (idiom, (bits, a)) =>
            val kNext = NN(OperatorLiteral(K(true :: bits)))
            ReductionPossibility("Push down",
              kNext(NN(Pure(idiom))(a))
            )
          }
        | AppLike(PureLike zip (Id as AppLike)) --/> { case (idiom, (r, (f, x))) =>
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
                              join(NN(Pure(idiom))(x))
                            ))
                        }
                    }
                  else
                    otherIdiom.predict match {
                      case None =>
                        // We must pass through, hopefully leading up to a sequence.
                        // We try not to get to this point; we like to pass through using pures,
                        // not anti-pures.
                        val a = idiom.app
                        Some(ReductionPossibility("Push down (3)",
                          a(NN(Pure(idiom))(f))(NN(Pure(idiom))(x))
                        )).pure[M]
                      case Some(predict) =>
                        val a = idiom.app
                        val p = idiom.pure
                        val map = s(k(a))(p)
                        Some(ReductionPossibility("Predict",
                          f(predict(map)(NN(Pure(idiom))(x)))
                        )).pure[M]
                    }
                }
              | PureLike -> { _ =>
                  val a = idiom.app
                  Some(ReductionPossibility("Push down (2)",
                    a(NN(Pure(idiom))(f))(NN(Pure(idiom))(x))
                  ))
                }
              | Id --> { _ =>
                  r.fullyApplied map (_ match {
                    case true => None
                    case false =>
                      val a = idiom.app

                      Some(ReductionPossibility("Push down (1)",
                        a(NN(Pure(idiom))(f))(NN(Pure(idiom))(x))
                      ))
                  })
                }
            ) map (_.join)
          }
      )
    }
  }

  def normalize(x: RNode): M[Option[RNode]] = {
    x.normalized
  }
}
