
package reductionengine.logic

object NodeUtils {
  def applyAll[T](t: Traversable[T]): Replacement[T] = t.toList.reverse match {
    case Nil =>
      NewNode(OperatorLiteral(B))
    case one :: Nil =>
      AlreadyThere(one)
    case cdr :: car =>
      NewNode(App(applyAll(car), AlreadyThere(cdr)))
  }

  def applyAllTo[T](to: Node[Replacement[T]], them: Traversable[T]): Node[Replacement[T]] =
    them.toList.reverse match {
      case Nil =>
        to
      case cdr :: car =>
        App(NewNode(applyAllTo(to, car)), AlreadyThere(cdr))
    }
}
