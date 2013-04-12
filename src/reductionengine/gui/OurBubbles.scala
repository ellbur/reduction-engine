
package reductionengine.gui

import collection.mutable.ArrayBuffer
import javax.swing.SwingWorker
import reactive.{EventStream, Signal, EventSource, Var}
import java.awt.Point
import ellbur.collection.reactivemap.clobber._
import ellbur.dependenttypes._
import scalaz._
import Scalaz._
import signalutils._
import languageFeature.postfixOps
import com.github.ellbur.collection.dependentmap.immutable.DependentMap

trait OurBubbles { this: Editor =>
  import sugar.logic.ReductionPossibility
  import sugar.logic

  case class EditedBubble(bubble: Bubble, editing: BubbleEditing)

  object editingState {
    lazy val visibleBubbles = new ClobberFRMapID[Bubble, BubbleEditing](addOrRemoveBubbles)
    lazy val focusedBubble = Var[Option[EditedBubble]](None)
    lazy val focusedParent = Var[Option[Bubble]](None)
    lazy val focusedChild = Var[Option[Bubble]](None)

    val addOrRemoveBubbles = new EventSource[Map[Bubble,BubbleEditing]]

    def get(b: Bubble): Signal[Option[BubbleEditing]] = visibleBubbles.get(b)

    def getEdited(b: Bubble): Signal[Option[EditedBubble]] = get(b) map (_ map { e =>
      EditedBubble(b, e)
    })

    lazy val freeze: Signal[FrozenEditingState] = visibleBubbles.toMap map {
      visibleBubbles =>
        FrozenEditingState(
          visibleBubbles.toList map { case (bubble, editing) =>
            FrozenEditedBubble(bubble, editing.freeze)
          }
        )
    }

    // TODO: This really shouldn't exist.
    def bubbleFor(editing: BubbleEditing): Option[Bubble] =
      visibleBubbles.toMap.now.toTraversable.find(_._2 == editing) map (_._1)
  }
  import editingState.focusedBubble

  case class FrozenEditedBubble(bubble: Bubble, editing: FrozenBubbleEditing)
  case class FrozenEditingState(bubbles: Seq[FrozenEditedBubble]) {
    def goTo() {
      println(s"Going to ${bubbles map (_.bubble)}")

      editingState.addOrRemoveBubbles.fire((bubbles map { edited =>
        (edited.bubble, edited.bubble.edit(edited.editing.x, edited.editing.y))
      }).toMap)

      jumpFocus(None)
    }
  }

  val history = ArrayBuffer[FrozenEditingState]()

  import sugar.{NewNode => NN}
  val K = NN(sugar.ApicalOperator(sugar.BasicOperator(logic.K(1, Seq(false))), Seq()))
  val S = NN(sugar.ApicalOperator(sugar.BasicOperator(logic.S(1)), Seq()))
  val standardIdiomKinds = Seq(
    sugar.KindOfIdiom("Var", K, S)
  )

  val wantsToBury = new EventSource[Unit]()
  val wantsToRecollect = new EventSource[Unit]()

  sealed trait DoOrClear
  case object Do extends DoOrClear
  case object Clear extends DoOrClear

  val buryChoices: Signal[Option[(Bubble, Seq[sugar.KindOfIdiom])]] =
    (((wantsToBury map (_ => Do)) | (focusedBubble.change map (_ => Clear))) map {
      case Clear => None
      case Do => focusedBubble.now map { here =>
        (here.bubble, standardIdiomKinds)
      }
    }).hold(None)

  val recollectChoices: Signal[Option[(Bubble, Seq[sugar.KindOfIdiom])]] =
    (((wantsToRecollect map (_ => Do)) | (focusedBubble.change map (_ => Clear))) map {
      case Clear => None
      case Do => focusedBubble.now map { here =>
        (here.bubble, standardIdiomKinds)
      }
    }).hold(None)

  type RPB = ReductionPossibility

  focusedBubble.change foreach { editedBubble =>
    updateFocusedReductions(editedBubble)
  }

  case class FocusedReductions(where: Bubble, reductions: Seq[ReductionPossibility])

  val focusedReductions = Var[Option[FocusedReductions]](None)
  def updateFocusedReductions(editingBubble: Option[EditedBubble]) {
    focusedReductions() = None

    editingBubble foreach { editingBubble =>
      import editingBubble.bubble

      val worker = new SwingWorker[Option[FocusedReductions], Any] {
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
