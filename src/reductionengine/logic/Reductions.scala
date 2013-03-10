
package reductionengine.logic

case class ReductionPossibility[N](name: String, remapping: Replacement[N])

sealed trait Replacement[N]
case class AlreadyThere[N<:NodeLike](is: N) extends Replacement[N]
case class NewNode[N<:NodeLike](is: Node[Replacement[N]]) extends Replacement[N]

object StandardReductions {
  def find[N<:NodeLike[N]](n: N): Option[ReductionPossibility[N]] = n match {
    case AppLike(AppLike(PlusLike, IntLiteralLike(a))) =>
      Some(ReductionPossibility(
        "Perform addition",
        NewNode(IntLiteral(a + b))
      ))

    case _ => None
  }
}
