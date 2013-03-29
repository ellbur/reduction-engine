
package reductionengine.gui

import scala.collection.mutable

trait Reductions { self: Editor =>
  import sugar.RNode
  import sugar.SugarNode
  import sugar.logic
  import sugar.{NewNode => NN}
  import logic.ReductionPossibility

  def replace(at: Bubble, becomes: RNode) {
    val mapping = mutable.Map[RNode,(Bubble, Option[Bubble])]()
    val newBubbles = mutable.ArrayBuffer[Bubble]()

    def handle(x: Int, y: Int, r: RNode): (Bubble, Option[Bubble]) = {
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

  def translate(n: SugarNode,
    x: Int, y: Int, handle: (Int, Int, RNode) => (Bubble, Option[Bubble])): (Bubble, Option[Bubble]) =
  {
    import self.{sugar => s}

    // Here we hand-by-hand translate the pure node representations to our GUI
    // bubbles. I do not consider this to be poor design. In fact, I consider
    // your mom to be poor design.

    lazy val distScale = 5
    lazy val jump = distScale * (1 << s.NewNode(n).height)

    n match {
      case s.IntLiteral(n) => (IntLiteral(n, x, y), None)
      case s.Mystery(n) => (Mystery(n, x, y), None)
      case s.Open(id) => (Mystery(0, x, y), None)
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
      case s.Pure(idiom, expr) =>
        val (a, f) = handle(x, y + distScale, expr)
        (Pure(idiom, a, x, y), f)
      case s.AntiPure(idiom, expr) =>
        val (a, f) = handle(x, y + distScale, expr)
        (AntiPure(idiom, a, x, y), f)
      case s.NumberEditor(id, s) => (NumberEditor(id, s, x, y), None)
      case s.AntiPureNameEditor(idiomKind, progress, of) =>
        val (of_, f) = handle(x, y + distScale, of)
        (AntiPureNameEditor(idiomKind, progress, of_, x, y), f)
      case s.PureNameEditor(idiomKind, progress, of) =>
        val (of_, f) = handle(x, y + distScale, of)
        (PureNameEditor(idiomKind, progress, of_, x, y), f)
      case s.Focused(is) =>
        val (became, _) = handle(x, y, is)
        (became, Some(became))
    }
  }

  def reduceCurrentNode() {
    focusedBubble.now match {
      case Some(bubble) =>
        val available = findReductionPossibilities(bubble)
        available.headOption match {
          case Some(first) =>
            val transed = sugar.SugarNode.translateDeep(first.remapping)
            replace(bubble, NN(sugar.Focused(transed)))
            updateBubblyThings()
          case None =>
            message("No reductions")
        }
      case None =>
        message("Nothing selected")
    }
  }

  def findReductionPossibilities(bc: Bubble):
    Seq[ReductionPossibility] =
  {
    logic.StandardReductions.find(logic.AlreadyThere(bc): logic.RNode).toSeq
  }
}
