
package reductionengine.logic

case class ReductionPossibility[R](name: String, remapping: Replacement[R])

sealed trait Replacement[+N]

case class AlreadyThere[+N](is: N) extends Replacement[N] {
  override def toString = "*"
}
case class NewNode[+N](is: Node[Replacement[N]]) extends Replacement[N] {
  override def toString = is.toString
}

trait NodeLike[S] {
  def toNode(t: Replacement[S]): Node[Replacement[S]]
}
object NodeLike {
  def toNode[T:NodeLike](x: Replacement[T]) = implicitly[NodeLike[T]].toNode(x)

  implicit object forNothing extends NodeLike[Nothing] {
    def toNode(t: Replacement[Nothing]) = t match {
      case NewNode(n) => n
      case AlreadyThere(nothing) => nothing
    }
  }
}
import NodeLike.toNode

import reductionengine.logic.{NewNode => NN}

sealed trait Mod {
  def apply[T](x: Replacement[T]): Replacement[T]
}
case class PureMod(of: Idiom) extends Mod {
  def apply[T](x: Replacement[T]): Replacement[T] = x match {
    case NewNode(AntiPure(`of`, it)) => it
    case it => NN(Pure(of, it))
  }
}
case class AntiPureMod(of: Idiom) extends Mod {
  def apply[T](x: Replacement[T]): Replacement[T] = x match {
    case NewNode(Pure(`of`, it)) => it
    case it => NN(AntiPure(of, it))
  }
}

case class ModList(of: List[Mod]) {
  def apply[T](x: Replacement[T]): Replacement[T] =
    ModList.applyAll(x, of)

  def ::(next: Mod) = this.copy(of = next :: of)
}
object ModList {
  def applyAll[T](x: Replacement[T], of: List[Mod]): Replacement[T] = of match {
    case Nil => x
    case of :: ofs => of(applyAll(x, ofs))
  }

  def forward(idioms: List[Idiom]): ModList = ModList(idioms map (PureMod(_)))
  def reverse(idioms: List[Idiom]): ModList = ModList(idioms.reverse map (AntiPureMod(_)))
}

object AppLike {
  def unapply[T:NodeLike](x: Replacement[T]): Option[(Child[T], Child[T])] =
    toNode(x) match {
      case App(idioms, car, cdr) =>
        val fwd = ModList.forward(idioms)
        val rev = ModList.reverse(idioms)
        Some((
          Child(fwd, rev(car)),
          Child(fwd, rev(cdr))
        ))
      case Pure(of, AppLike(Child(carMods, car), Child(cdrMods, cdr))) =>
        Some((
          Child(PureMod(of) :: carMods, car),
          Child(PureMod(of) :: cdrMods, cdr)
        ))
      case AntiPure(of, AppLike(Child(carMods, car), Child(cdrMods, cdr))) =>
        Some((
          Child(AntiPureMod(of) :: carMods, car),
          Child(AntiPureMod(of) :: cdrMods, cdr)
        ))
      case _ =>
        None
    }

  // TODO
  def idiomMods[T](idioms: List[Idiom]): (Replacement[T] => Replacement[T]) = ???
}

case class Child[T](mods: ModList, is: Replacement[T])

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
      // Let us ignore all these for now, until we can get just "I" to work!
      // I'm starting with "I" because it is non-strict (ie it can be evaluated in an applicative environment)
      // and it is very, very simple.
      case AppLike(
        Child(_, OperatorLiteralLike(I)),
        Child(m, x)
      ) =>
        Some(ReductionPossibility(
          "Reduce I",
          m(x)
        ))
      // The E-J rule is here not because it is useful for anything but just because it provides a good
      // demonstration of idiomatic reductions.
      case AppLike(
        Child(_, OperatorLiteralLike(E)),
        Child(m1, AppLike(
          Child(_, OperatorLiteralLike(J)),
          Child(m2, x)
        ))
      ) =>
        Some(ReductionPossibility(
          "Reduce E-J",
          m1(m2(x))
        ))
      /*
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
      */
      case _ => None
    }
  }
}
