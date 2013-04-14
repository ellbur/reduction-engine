
package reductionengine.gui

import collection.mutable
import reactive.{EventSource, EventStream}
import ellbur.collection.reactivemap.clobber._
import com.github.ellbur.collection.dependentmap.immutable._
import graphutils.GraphLayout4

trait Reductions { self: Editor =>
  import sugar.RNode
  import sugar.SugarNode
  import sugar.logic
  import sugar.{NewNode => NN}
  import logic.ReductionPossibility

  case class ReplaceResults(
    site: Bubble,
    replacement: Bubble,
    totallyNew: Traversable[Bubble],
    oldButDisturbed: Traversable[Bubble],
    focused: Bubble)

  /** Represents the action of replacing. */
  lazy val replace: EventStream[ReplaceResults] = doReplace map { case (at, becomes) =>
    val totallyNew = mutable.Set[Bubble]()
    val oldButDisturbed = mutable.Set[Bubble]()
    val mapping = mutable.Map[RNode, (Bubble, Option[Bubble])]()

    def handle(it: RNode): (Bubble, Option[Bubble]) = mapping.getOrElseUpdate(it, it match {
      case sugar.AlreadyThere(it) =>
        oldButDisturbed += it
        (it, None)
      case sugar.NewNode(node, _) =>
        val (bub, focus) = translate(node, handle _)
        totallyNew += bub
        (bub, focus)
    })

    val (next, focused) = handle(becomes)
    val reallyFocused = focused getOrElse next

    ReplaceResults(at, next, totallyNew, oldButDisturbed, reallyFocused)
  }

  def translate(n: SugarNode,
    handle: RNode => (Bubble, Option[Bubble])): (Bubble, Option[Bubble]) =
  {
    import self.{sugar => s}

    // Here we hand-by-hand translate the pure node representations to our GUI
    // bubbles. I do not consider this to be poor design. In fact, I consider
    // your mom to be poor design.
    n match {
      case s.IntLiteral(n) => (IntLiteral(n), None)
      case s.Mystery(n) => (Mystery(n), None)
      case s.Open(id) => (Mystery(0), None)
      case s.Root(is) =>
        val (a, f) = handle(is)
        (Root(a), f)
      case s.ApicalOperator(op, args) =>
        val (fArgs, bArgs) = {
          args map (handle(_))
        }.unzip
        (ApicalOperator(op, fArgs), bArgs.flatten.headOption)
      case s.Pure(idiom, expr) =>
        val (a, f) = handle(expr)
        (Pure(idiom, a), f)
      case s.AntiPure(idiom, expr) =>
        val (a, f) = handle(expr)
        (AntiPure(idiom, a), f)
      case s.NumberEditor(id, s) => (NumberEditor(id, s), None)
      case s.AntiPureNameEditor(idiomKind, progress, of) =>
        val (of_, f) = handle(of)
        (AntiPureNameEditor(idiomKind, progress, of_), f)
      case s.PureNameEditor(idiomKind, progress, of) =>
        val (of_, f) = handle(of)
        (PureNameEditor(idiomKind, progress, of_), f)
      case s.VariableEditor(id, s) => (VariableEditor(id, s), None)
      case s.Variable(name) => (Variable(name), None)
      case s.Focused(is) =>
        val (became, _) = handle(is)
        (became, Some(became))
    }
  }

  val doReplace = new EventSource[(Bubble, RNode)]

  val reduceCurrentNode = Action("Reduce", Hard) {
    editingState.focusedBubble.now match {
      case Some(editedBubble) =>
        import editedBubble.bubble
        val available = findReductionPossibilities(bubble)
        available.headOption match {
          case Some(first) =>
            val translated = sugar.SugarNode.translateDeep(first.remapping)
            doReplace.fire((bubble, translated))
            DescribedAs(first.name)
          case None =>
            message("No reductions")
            NotPerformed
        }
      case None =>
        message("Nothing selected")
        NotPerformed
    }
  }

  def findReductionPossibilities(bc: Bubble):
    Seq[ReductionPossibility] =
  {
    logic.StandardReductions.find(logic.AlreadyThere(bc): logic.RNode).toSeq
  }

  // Introduces new bubbles into the mix.
  val replacePerformed: EventStream[(Map[Bubble, BubbleEditing], Bubble)] = replace map { r =>
    import r.{totallyNew, oldButDisturbed, site, replacement, focused}
    val disturbedSet = oldButDisturbed.toSet

    val current = editingState.visibleBubbles.toMap.now
    println(s"Had ${current map (_._1)}")

    val remapping = mutable.Map[Bubble, Option[Bubble]]()
    val relegatedEditings = mutable.Map[Bubble, BubbleEditing]()

    def handle(bubble: Bubble, editing: Option[BubbleEditing]): Option[Bubble] = {
      remapping.getOrElseUpdate(bubble, {
        editing foreach { editing => relegatedEditings += ((bubble, editing)) }
        if (bubble == site) {
          current.get(replacement) foreach { e =>
            relegatedEditings += ((replacement, e))
          }
          Some(replacement)
        }
        else {
          val children = bubble.children
          var changed: Boolean = false
          val changedChildren = children map { child =>
            handle(child, current get child) match {
              case Some(next) =>
                changed = true
                next
              case _ =>
                child
            }
          }
          if (changed) {
            val next = bubble.withChildren(changedChildren)
            editing foreach { editing => relegatedEditings += ((next, editing)) }
            Some(next)
          }
          else {
            None
          }
        }
      })
    }

    val visibleTransformed = current map {
      case (bubble, editing) =>
        handle(bubble, Some(editing)) getOrElse bubble
    }
    println(s"Transformed them to ${visibleTransformed}")

    val reachable = mutable.Set[Bubble]()
    def makeReachable(b: Bubble) {
      if (!(reachable contains b)) {
        reachable += b
        for (c <- b.children)
          makeReachable(c)
      }
    }
    for (bubble <- visibleTransformed) bubble match {
      case root: Root => makeReachable(root)
      case _ =>
    }
    println(s"Found that ${reachable.toList} were reachable")

    val allThings = (reachable map { bubble =>
      (bubble, relegatedEditings.get(bubble))
    }).toIndexedSeq

    val bubbleIndices = allThings.map(_._1).zipWithIndex.toMap

    def screenCenter = (
      canvas.getX.toDouble + canvas.getWidth.toDouble/2,
      canvas.getY.toDouble + canvas.getHeight.toDouble/2
    )
    val center = (current.get(site) map (p => (p.x.now.toDouble, p.y.now.toDouble))
      getOrElse screenCenter)

    val initial = allThings map { case (bubble, optionalEditing) =>
      GraphLayout4.Node(
        origPosition = optionalEditing map (editing =>
          (editing.x.now.toDouble, editing.y.now.toDouble)
        ),
        disturb = disturbedSet contains bubble,
        // Note: this assumes that there should be no way to have non-indexed children.
        adjacency = bubble.children map (bubbleIndices(_))
      )
    }

    val coordinates = GraphLayout4.computeLayout(
      40.0,
      center,
      initial
    )

    val next = (allThings.zipWithIndex map { case ((bubble, optionalEditing), index) =>
      val (newX, newY) = coordinates(index)
      (bubble, optionalEditing match {
        case Some(editing) =>
          editing.x() = newX.toInt
          editing.y() = newY.toInt
          editing
        case None =>
          bubble.edit(newX.toInt, newY.toInt)
      })
    }).toMap

    (next, focused)
  }

  replacePerformed foreach { case (mapping, focus) =>
    editingState.addOrRemoveBubbles.fire(mapping)
    jumpFocus(Some(focus))
  }
}
