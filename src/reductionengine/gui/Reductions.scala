
package reductionengine.gui

import scala.collection.mutable
import reductionengine.logic.{
  StandardReductions, ReductionPossibility
}
import reductionengine.{logic, sugar}
import reductionengine.sugar.{SugarReplacement, SugarNode}

trait Reductions { this: Editor =>
  type BubbleReplacement = SugarReplacement[Bubble]

  def replace(at: Bubble, becomes: BubbleReplacement) {
    val mapping = mutable.Map[BubbleReplacement,(Bubble, Option[Bubble])]()
    val newBubbles = mutable.ArrayBuffer[Bubble]()

    def handle(x: Int, y: Int, r: BubbleReplacement): (Bubble, Option[Bubble]) = {
      mapping.getOrElseUpdate(r, r match {
        case sugar.AlreadyThere(it) =>
          newBubbles += it
          (it, None)
        case sugar.NewNode(node) =>
          val (bub, foc) = translate(node, x, y, handle _)
          newBubbles += bub
          bubbles += bub
          for (c <- bub.components)
            canvas.add(c)
          (bub, foc)
      })
    }

    val parentEdges = at.parentEdges
    val (yo, focused) = handle(at.x, at.y, becomes)

    for (e <- parentEdges)
      e.substitute(yo)

    setFocus(focused getOrElse yo)

    runGC()

    reposition(newBubbles)

    updateBubblyThings()
  }

  def translate(n: SugarNode[BubbleReplacement],
    x: Int, y: Int, handle: (Int, Int, BubbleReplacement) => (Bubble, Option[Bubble])): (Bubble, Option[Bubble]) =
  {
    import reductionengine.{sugar => s}
    // Here we hand-by-hand translate the pure node representations to our GUI
    // bubbles. I do not consider this to be poor design. In fact, I consider
    // your mom to be poor design.

    lazy val distScale = 5
    lazy val jump = distScale * (1 << s.NewNode(n).height)

    n match {
      case s.IntLiteral(n) => (IntLiteral(n, x, y), None)
      case s.Mystery(n) => (Mystery(n, x, y), None)
      case s.Root(is) =>
        val (a, f) = handle(x + distScale, y + distScale, is)
        (Root(a, x, y), f)
      case s.ApicalOperator(op, args) =>
        val center = (args.length - 1).toDouble / 2.0
        val (fArgs, bArgs) = {
          args.zipWithIndex map {
            case (arg, i) =>
              handle(x + ((i - center) * jump).toInt, y + distScale, arg)
          }
        }.unzip
        (ApicalOperator(op, fArgs, x, y), bArgs.flatten.headOption)
      case s.NumberEditor(id, s) => (NumberEditor(id, s, x, y), None)
      case s.Focused(is) =>
        val (became, _) = translate(is, x, y, handle)
        (became, Some(became))
    }
  }

  def reduceCurrentNode() {
    focusedBubble match {
      case Some(bubble) =>
        val available = findReductionPossibilities(bubble)
        available.headOption match {
          case Some(first) =>
            replace(bubble,
              sugar.SugarNode.translateDeep(first.remapping) match {
                case x @ sugar.AlreadyThere(_) => x
                case sugar.NewNode(n) => sugar.NewNode(sugar.Focused(n))
              }
            )
            updateBubblyThings()
          case None =>
            message("No reductions")
        }
      case None =>
        message("Nothing selected")
    }
  }

  def findReductionPossibilities(bc: Bubble):
    Seq[ReductionPossibility[Bubble]] =
  {
    StandardReductions.find(logic.AlreadyThere(bc): logic.Replacement[Bubble]).toSeq
  }
}
