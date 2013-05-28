
package reductionengine.gui

import javax.swing._
import java.awt._
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
import scala.Some
import redosignals._
import RedoSignals._

trait Layout { this: Editor =>
  class MainCanvas extends JPanel with SetupInputs {
    val rootFlagColor = new Color(255, 230, 0)
    val rootFlagStroke = new BasicStroke(2)

    val parentEdgeColor = new Color(100, 200, 100)
    val childEdgeColor = new Color(150, 200, 150)
    val neutralEdgeColor = new Color(100, 100, 100)

    val strongEdgeStroke = new BasicStroke(3)
    val neutralEdgeStroke = new BasicStroke(1)

    override def paintComponent(_g: Graphics) {
      val g = _g.asInstanceOf[Graphics2D]

      g.setColor(Color.white)
      g.fillRect(0, 0, getSize().width, getSize().height)

      val bubbles = editingState.visibleBubbles.now

      val focused = editingState.focusedBubble.now
      val focusedParent = editingState.focusedParent.now
      val focusedChild = editingState.focusedChild.now

      def is[A](x: A, y: Option[A]) =
        y map (_ == x) getOrElse false

      // Render bubbles
      val bubbleRenderings = (bubbles map { bubble =>
        (bubble, bubble.render(g))
      }).toMap

      // Render edges
      bubbleRenderings foreach {
        case (from, r) =>
          val can = from.children.now.toIndexedSeq
          val want = r.childAttachments.toIndexedSeq

          for (i <- 0 to can.length-1) {
            val fromWhere = want(i)
            val to = can(i)
            bubbleRenderings.get(to) match {
              case Some(it) =>
                val toWhere = it.parentAttachment

                if (is(from, focusedParent) && is(to, focused)) {
                  g.setColor(parentEdgeColor)
                  g.setStroke(strongEdgeStroke)
                }
                else if (is(to, focusedChild) && is(from, focused)) {
                  g.setColor(childEdgeColor)
                  g.setStroke(strongEdgeStroke)
                }
                else {
                  g.setColor(neutralEdgeColor)
                  g.setStroke(neutralEdgeStroke)
                }
                g.drawLine(fromWhere.x, fromWhere.y, toWhere.x, toWhere.y)
              case None =>
                println(s"Don't seem to have rendered child $to")
            }
          }
          // TODO: Indicate empty spots.
      }

      // Render roots
      editingState.roots.now foreach { rootBubble =>
        bubbleRenderings.get(rootBubble) foreach { rendering =>
          val (x, y) = (rendering.bounds.x, rendering.bounds.y)
          val flagSpot = (x-8, y-8)
          g.setColor(rootFlagColor)
          g.setStroke(rootFlagStroke)
          g.drawLine(flagSpot._1-3, flagSpot._2, flagSpot._1+3, flagSpot._2)
          g.drawLine(flagSpot._1, flagSpot._2-3, flagSpot._1, flagSpot._2+3)
        }
      }

      renderedBubbles.clear()
      renderedBubbles ++= bubbleRenderings map {
        case (b, r) => RenderedBubble(b, r.bounds)
      }
    }

    setLayout(null)
  }

  (
        (editingState.visibleBubbles)
    zip (editingState.focusedBubble)
    zip (editingState.focusedChild)
    zip (editingState.focusedParent)
  ) foreach { _ =>
    canvas.repaint()
  }

  lazy val messageLabel = new JLabel("Ready.")
  lazy val canvas = new MainCanvas

  setLayout(new MigLayout((new LC).insets("0")))
  add(canvas, (new CC).cell(0, 0).grow().push())
  add(undoRedoSidebar, (new CC).cell(1, 0).growY().pushY())
  add(messageLabel, (new CC).cell(0, 1).spanX().growX())

  def message(text: String) {
    messageLabel setText text
  }

  case class RenderedBubble(bubble: Bubble, bounds: Rectangle)
  var renderedBubbles = mutable.ArrayBuffer[RenderedBubble]()

  def bubbleAt(x: Int, y: Int): Option[Bubble] =
    (renderedBubbles.reverse filter (_.bounds contains (x, y))).headOption map (_.bubble)

  lazy val activePopups =
    Seq(reductionsPopup).sequence map (_.flatten)

  val popupPosition = editingState.focusedBubble flatMap {
    case None =>
      Point(10, 10).pure[Target]
    case Some(bubble) =>
      bubble.location
  }

  activePopups zip popupPosition foreach { case (popups, Point(x, y)) =>
    var here: Int = y
    for (popup <- popups) {
      popup.setLocation(x + 40, here - popup.getHeight)
      here += popup.getHeight + 10
    }
    repaint()
  }

  lazy val reductionsComponent = new JLabel
  reductionsComponent.setBorder(new TitledBorder("Reductions"))
  lazy val reductionsPopup = focusedReductions map {
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
  lazy val buryPopup = buryChoices map {
    case Some((where, idioms)) =>
      buryComponent.removeAll()
      idioms.zipWithIndex foreach { case (idiom, i) =>
        buryComponent.add(makeAction(idiom.name, addAntiPure(idiom, where)))
      }
      val Point(x, y) = popupPosition.now
      buryComponent.show(canvas, x + 40, y)

      Some(buryComponent)
    case _ =>
      buryComponent.setVisible(false)
      None
  }
  buryPopup foreach (_ => ())

  lazy val recollectComponent = new JPopupMenu
  lazy val recollectPopup = recollectChoices map {
    case Some((where, idioms)) =>
      recollectComponent.removeAll()
      idioms foreach { idiom =>
        recollectComponent.add(makeAction(idiom.name, addPure(idiom, where)))
      }
      val Point(x, y) = popupPosition.now
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

  // Components introduced by way of bubbles.
  lazy val allChildComponents = editingState.visibleBubbles map (_ flatMap (_.components))
  allChildComponents foreach { children =>
    canvas.removeAll()
    children foreach (canvas.add(_))
    children foreach (_.setFocusTraversalKeysEnabled(false))
  }
}
