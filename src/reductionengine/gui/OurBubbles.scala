
package reductionengine.gui

import collection.mutable.ArrayBuffer
import javax.swing.SwingWorker
import scalaz._
import Scalaz._
import languageFeature.postfixOps
import scala.collection.mutable
import redosignals._
import RedoSignals._
import javax.swing.text.html.HTMLDocument

trait OurBubbles { this: Editor =>
  import sugar.logic.ReductionPossibility
  import sugar.logic

  object editingState {
    val visibleBubbles = new Source[Set[Bubble]](Set())
    val roots = new Source[Set[Bubble]](Set())
    val focusedBubble = new Source[Option[Bubble]](None)
    val focusedParent = new Source[Option[Bubble]](None)
    val focusedChild = new Source[Option[Bubble]](None)

    lazy val freeze = visibleBubbles flatMap {
      visibleBubbles =>
        for {
          frozenVisible <- (visibleBubbles.toList map (_.freeze)).sequence
          idMap = (frozenVisible zip (frozenVisible map (_.id))).toMap
          frozenRoots <- roots map (_ map (_.id))
        } yield {
          FrozenEditingState(frozenVisible, frozenRoots)
        }
    }
  }
  import editingState.focusedBubble

  case class FrozenEditingState(bubbles: Traversable[FrozenBubble], roots: Traversable[Int]) {
    def goTo() {
      val idMap = (bubbles.toSeq map (_.id) zip bubbles.toSeq).toMap

      val remapping = mutable.Map[Int, Bubble]()
      def remap(f: FrozenBubble): Bubble = remapping.getOrElseUpdate(f.id, {
        manifest(f.node, f.loc, f.children map (remapId(_)))
      })
      def remapId(id: Int) = remap(idMap(id))

      editingState.visibleBubbles() = (bubbles map (remap(_))).toSet
      editingState.roots() = (roots map (remapId(_))).toSet

      jumpFocus(None)
    }
  }

  val history = ArrayBuffer[FrozenEditingState]()

  import sugar.{NewNode => NN}
  val K = NN(sugar.ApicalOperator(sugar.BasicOperator(logic.K(1, Seq(false))), Seq()))
  val S = NN(sugar.ApicalOperator(sugar.BasicOperator(logic.S(1)), Seq()))
  val standardIdiomKinds = Seq(
    sugar.standardIdiomKinds.lambda
  )

  sealed trait DoOrClear
  case object Do extends DoOrClear
  case object Clear extends DoOrClear

  val buryChoices = new Source[Option[(Bubble, Seq[sugar.KindOfIdiom])]](None)
  def openBuryChoices() {
    buryChoices() = focusedBubble.now map { here => (here, standardIdiomKinds) }
  }
  focusedBubble.changed foreach { _ =>
    buryChoices() = None
  }

  val recollectChoices = new Source[Option[(Bubble, Seq[sugar.KindOfIdiom])]](None)
  def openRecollectChoices() {
    recollectChoices() = focusedBubble.now map { here => (here, standardIdiomKinds) }
  }
  focusedBubble.changed foreach { _ =>
    recollectChoices() = None
  }

  focusedBubble foreach { editedBubble =>
    updateFocusedReductions(editedBubble)
  }

  case class FocusedReductions(where: Bubble, reductions: Seq[ReductionPossibility])

  lazy val focusedReductions = new Source[Option[FocusedReductions]](None)
  def updateFocusedReductions(bubble: Option[Bubble]) {
    focusedReductions() = None

    bubble foreach { bubble =>
      val worker = new SwingWorker[Option[FocusedReductions], Nothing] {
        def doInBackground() = {
          Some(FocusedReductions(
            bubble,
            findReductionPossibilities(bubble)
          ))
        }

        override def done() {
          val were = get()
          focusedReductions() = were
        }
      }
      worker.execute()
    }
  }
}
