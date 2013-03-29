
package reductionengine.gui

import collection.mutable.ArrayBuffer
import javax.swing.SwingWorker
import reactive.Var

trait OurBubbles { this: Editor =>
  import sugar.logic.ReductionPossibility
  import sugar.{NewNode => NN, AlreadyThere => AT}
  import sugar.logic

  val roots = ArrayBuffer[Bubble]()
  val bubbles = ArrayBuffer[Bubble]()

  val focusedBubble = Var[Option[Bubble]](None)
  val focusedChild = Var[Option[Bubble]](None)
  val focusedParent = Var[Option[Bubble]](None)

  val K = NN(sugar.ApicalOperator(sugar.BasicOperator(logic.K(1, Seq(false))), Seq()))
  val S = NN(sugar.ApicalOperator(sugar.BasicOperator(logic.S(1)), Seq()))
  val standardIdiomKinds = Seq(
    sugar.KindOfIdiom("Var", K, S)
  )

  val buryChoices = Var[Option[(Bubble, Seq[sugar.KindOfIdiom])]](None)
  def updateBuryChoices() {
    focusedBubble.now foreach { here =>
      buryChoices() = Some((here, standardIdiomKinds))
    }
  }
  def clearBuryChoices() { buryChoices() = None }

  val recollectChoices = Var[Option[(Bubble, Seq[sugar.KindOfIdiom])]](None)
  def updateRecollectChoices() {
    focusedBubble.now foreach { here =>
      recollectChoices() = Some((here, standardIdiomKinds))
    }
  }
  def clearRecollectChoices() { recollectChoices() = None }

  focusedBubble.change foreach { now =>
    clearBuryChoices()
    clearRecollectChoices()
  }

  def setFocus(b: Bubble) {
    focusedBubble() = Some(b)
    focusedChild() = identifyAChild(b)
    focusedParent() = identifyAParent(b)
    b.receiveFocus()
  }

  def updateBubblyThings() {
    updateFocusedReductions()
  }

  type RPB = ReductionPossibility

  val focusedReductions = Var[Option[(Bubble, Seq[RPB])]](None)
  def updateFocusedReductions() {
    println(1)
    focusedReductions() = None
    val worker = new SwingWorker[Option[(Bubble, Seq[RPB])],Any] {
      def doInBackground = {
        println(2)
        focusedBubble.now map { bubble =>
          (bubble, findReductionPossibilities(bubble))
        }
      }

      override def done() {
        val were = get()
        focusedReductions() = were
      }
    }
    worker.execute()
  }

  /**
   * Move some bubbles around in (x, y) in order
   */
  def reposition(toMoveP: Seq[Bubble]) {
    import graphutils.{GraphLayout4 => GL}

    val moved = toMoveP.toSet

    val bubbleToK = bubbles.zipWithIndex.toMap

    case class Datum(x: Double, y: Double, adj: Seq[Int], dist: Boolean)
    val data = bubbles map { b =>
      Datum(b.x, b.y, (b.children map (bubbleToK(_))).toSeq, moved contains b)
    }

    val scale = 40.0
    val origPosition = data map (d => (d.x, d.y))
    val adjacency = data map (_.adj)
    val disturbed = data map (_.dist)

    val newPos = GL.computeLayout(scale=scale, origPosition=origPosition,
      adjacency=adjacency, disturbed=disturbed)

    newPos zip bubbles foreach {
      case ((x, y), b) =>
        b.move((x - b.x).toInt, (y - b.y).toInt)
    }
  }
}
