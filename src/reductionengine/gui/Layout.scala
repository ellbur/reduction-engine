
package reductionengine.gui

import javax.swing._
import java.awt.{BasicStroke, Color, Graphics2D, Graphics}
import net.miginfocom.swing.MigLayout
import net.miginfocom.layout.{CC, LC}
import scala.collection.mutable
import javax.swing.border.TitledBorder
import reactive.{EventStream, Signal}
import scala.Some
import java.awt.event.ActionEvent
import scalaz._
import Scalaz._
import signalutils._

trait Layout { this: Editor =>
  class MainCanvas extends JPanel with SetupInputs {
    override def paintComponent(_g: Graphics) {
      // TODO
      val g = _g.asInstanceOf[Graphics2D]

      g.setColor(Color.white)
      g.fillRect(0, 0, getSize().width, getSize().height)

      val editedBubbles = editingState.visibleBubbles.toMap.now map {
        case (bubble, editing) => EditedBubble(bubble, editing)
      }

      val focused = editingState.focusedBubble.now map (_.bubble)
      val focusedParent = editingState.focusedParent.now
      val focusedChild = editingState.focusedChild.now

      def is[A](x: A, y: Option[A]) =
        y map (_ == x) getOrElse false

      // Render bubbles
      val bubbleRenderings = (editedBubbles map { edited =>
        val hasFocus = is(edited.bubble, focused)
        (edited.bubble, edited.editing.render(g, hasFocus))
      }).toMap

      // Render edges
      bubbleRenderings foreach {
        case (from, r) =>
          val can = from.children.toIndexedSeq
          val want = r.childAttachments.toIndexedSeq

          for (i <- 0 to can.length-1) {
            val fromWhere = want(i)
            val to = can(i)
            bubbleRenderings.get(to) match {
              case Some(it) =>
                val toWhere = it.parentAttachment

                if (is(from, focusedParent) && is(to, focused)) {
                  g.setColor(new Color(100, 200, 100))
                  g.setStroke(new BasicStroke(3))
                }
                else if (is(to, focusedChild) && is(from, focused)) {
                  g.setColor(new Color(150, 200, 150))
                  g.setStroke(new BasicStroke(3))
                }
                else {
                  g.setColor(new Color(0, 0, 0))
                  g.setStroke(new BasicStroke(1))
                }
                g.drawLine(fromWhere.x, fromWhere.y, toWhere.x, toWhere.y)
              case None =>
                println(s"Don't seem to have rendered child $to")
            }
          }
          // TODO: Indicate empty spots.
      }

      renderedBubbles.clear()
      renderedBubbles ++= bubbleRenderings map {
        case (b, r) => RenderedBubble(b, r.bounds)
      }
    }

    setLayout(null)
  }

  (
      (editingState.addOrRemoveBubbles map (_ => ()))
    | (editingState.focusedBubble.change map (_ => ()))
    | (editingState.focusedChild.change map (_ => ()))
    | (editingState.focusedParent.change map (_ => ()))
  ) foreach { _ =>
    canvas.repaint()
  }

  val messageLabel = new JLabel("Ready.")
  val canvas = new MainCanvas

  setLayout(new MigLayout((new LC).insets("0")))
  add(canvas, (new CC).cell(0, 0).grow().push())
  add(undoRedoSidebar, (new CC).cell(1, 0).growY().pushY())
  add(messageLabel, (new CC).cell(0, 1).spanX().growX())

  def message(text: String) {
    messageLabel setText text
  }

  var renderedBubbles = mutable.ArrayBuffer[RenderedBubble]()

  def bubbleAt(x: Int, y: Int): Option[Bubble] =
    (renderedBubbles.reverse filter (_.bounds contains (x, y))).headOption map (_.bubble)

  lazy val activePopups: Signal[Seq[JComponent]] =
    Seq(reductionsPopup).sequence map (_.flatten)

  val popupPosition = editingState.focusedBubble flatMap {
    case None =>
      (10, 10).pure
    case Some(EditedBubble(bubble, editing)) =>
      editing.x zip editing.y
  }

  activePopups zip popupPosition foreach { case (popups, (x, y)) =>
    var here: Int = y
    for (popup <- popups) {
      popup.setLocation(x + 40, here - popup.getHeight)
      here += popup.getHeight + 10
    }
    repaint()
  }

  lazy val reductionsComponent = new JLabel
  reductionsComponent.setBorder(new TitledBorder("Reductions"))
  lazy val reductionsPopup: Signal[Option[JComponent]] = focusedReductions map {
    case Some(FocusedReductions(bubble, possibilities)) if possibilities.length > 0 =>
      val rows = possibilities.toIndexedSeq.zipWithIndex map {
        case (reduction, i) =>
          <tr>
            <td>{if (i==0) "r" else ""}</td>
            <td>{(i+1).toString}</td>
            <td>{reduction.name}</td>
          </tr>
      }

      val html = <html><table>{rows}</table></html>

      reductionsComponent.setText(html.toString)
      reductionsComponent.setSize(reductionsComponent.getPreferredSize)
      canvas.add(reductionsComponent)

      Some(reductionsComponent)
    case _ =>
      canvas.remove(reductionsComponent)
      None
  }

  lazy val buryComponent = new JPopupMenu
  lazy val buryPopup: Signal[Option[JComponent]] = buryChoices map {
    case Some((where, idioms)) =>
      buryComponent.removeAll()
      idioms.zipWithIndex foreach { case (idiom, i) =>
        buryComponent.add(makeAction(idiom.name, addAntiPure(idiom, where)))
      }
      val (x, y) = popupPosition.now
      buryComponent.show(canvas, x + 40, y)

      Some(buryComponent)
    case _ =>
      println("no bury!")
      buryComponent.setVisible(false)
      None
  }
  buryPopup foreach (_ => ())

  lazy val recollectComponent = new JPopupMenu
  lazy val recollectPopup: Signal[Option[JComponent]] = recollectChoices map {
    case Some((where, idioms)) =>
      recollectComponent.removeAll()
      idioms foreach { idiom =>
        recollectComponent.add(makeAction(idiom.name, addPure(idiom, where)))
      }
      val (x, y) = popupPosition.now
      recollectComponent.show(canvas, x + 40, y)

      Some(recollectComponent)
    case _ =>
      recollectComponent.setVisible(false)
      None
  }
  recollectPopup foreach (_ => ())

  def makeAction(name: String, doIt: => Unit) = new AbstractAction() {
    putValue(javax.swing.Action.NAME, name)
    def actionPerformed(p1: ActionEvent) { doIt }
  }
}
