
package reductionengine.logic

trait NodeUtils { self: Logic with Reductions =>
  def applyAll(t: Traversable[RNode]): RNode = t.toList.reverse match {
    case Nil =>
      NewNode(OperatorLiteral(B))
    case one :: Nil =>
      one
    case cdr :: car =>
      NewNode(App(applyAll(car), cdr))
  }

  def applyAllTo(to: Node, them: Traversable[RNode]): Node = {
    def iter(things: List[RNode]): Node = things match {
      case Nil =>
        to
      case cdr :: car =>
        App(NewNode(iter(car)), cdr)
    }
    iter(them.toList.reverse)
  }
}
