
package reductionengine.gui

import collection.mutable.ArrayBuffer
import javax.swing.SwingWorker
import reductionengine.logic.ReductionPossibility
import reductionengine.logic
import reductionengine.sugar.SugarReplacement

trait OurBubbles { this: Editor =>
  val roots = ArrayBuffer[Bubble]()
  val bubbles = ArrayBuffer[Bubble]()

  var focusedBubble = Option[Bubble](null)
  var focusedChild = Option[Bubble](null)
  var focusedParent = Option[Bubble](null)

  def setFocus(b: Bubble) {
    focusedBubble = Some(b)
    focusedChild = identifyAChild(b)
    focusedParent = identifyAParent(b)
    b.receiveFocus()
  }

  def updateBubblyThings() {
    updateFocusedReductions()
  }

  type RPB = ReductionPossibility[Bubble]

  var focusedReductions: Traversable[RPB] = Seq()
  def updateFocusedReductions() {
    focusedReductions = Seq()
    val worker = new SwingWorker[Traversable[RPB],Any] {
      def doInBackground =
        focusedBubble.toTraversable flatMap (findReductionPossibilities(_))

      override def done() {
        focusedReductions = get()
        repaint()
      }
    }
    worker.execute()
  }

  /**
   * Move some bubbles around in (x, y) in order
   */
  def reposition(toMoveP: Seq[Bubble]) {
    import graphutils.{GraphLayout3 => GL}
    import GL.{Child, MovableNode}
    import GL.{FixedNode, FixedChild, MovableChild}

    val toMove = toMoveP.intersect(bubbles).toIndexedSeq
    val fixed = bubbles -- toMove

    val toMoveMap: Map[Bubble,Int] = toMove.zipWithIndex.toMap
    val fixedMap = fixed.zipWithIndex.toMap

    def bubbleToChild(b: Bubble): Child =
      toMoveMap.get(b) match {
        case Some(n) => MovableChild(n)
        case None => FixedChild(fixedMap(b))
      }

    val movableNodes = (toMove map { bubble =>
      MovableNode(bubble.x, bubble.y, (bubble.children map bubbleToChild _).toSeq)
    }).toIndexedSeq

    val fixedNodes = (fixed map { bubble =>
      FixedNode(bubble.x, bubble.y, (bubble.children map bubbleToChild _).toSeq)
    }).toIndexedSeq

    val newPositions = GL.computeLayout(movableNodes, fixedNodes, 30.0, 50.0)

    toMove zip newPositions foreach {
      case (bubble, pos) =>
        bubble.move(pos.x.toInt - bubble.x, pos.y.toInt - bubble.y)
    }
  }
}
