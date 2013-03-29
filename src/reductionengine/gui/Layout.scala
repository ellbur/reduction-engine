
package reductionengine.gui

import javax.swing._
import java.awt.{BasicStroke, Color, Graphics2D, Graphics}
import net.miginfocom.swing.MigLayout
import net.miginfocom.layout.{CC, LC}
import scala.collection.mutable
import javax.swing.border.TitledBorder
import reactive.Signal
import scala.Some
import java.awt.event.ActionEvent

trait Layout { this: Editor =>
  class MainCanvas extends JPanel with SetupInputs {
    override def paintComponent(_g: Graphics) {
      val g = _g.asInstanceOf[Graphics2D]

      g.setColor(Color.white)
      g.fillRect(0, 0, getSize().width, getSize().height)

      // Render bubbles
      val renderings = bubbles map { b =>
        b.render(g)
      }
      val bubbleToRendering = (bubbles zip renderings).toMap

      // Render edges
      bubbles zip renderings foreach {
        case (from, r) =>
          val can = from.children.toIndexedSeq
          val want = r.childAttachments.toIndexedSeq

          for (i <- 0 to can.length-1) {
            val fromWhere = want(i)
            val to = can(i)
            val toWhere = bubbleToRendering(to).parentAttachment

            if (from.isFocusedParent && to.hasFocus) {
              g.setColor(new Color(100, 200, 100))
              g.setStroke(new BasicStroke(3))
            }
            else if (to.isFocusedChild && from.hasFocus) {
              g.setColor(new Color(150, 200, 150))
              g.setStroke(new BasicStroke(3))
            }
            else {
              g.setColor(new Color(0, 0, 0))
              g.setStroke(new BasicStroke(1))
            }
            g.drawLine(fromWhere.x, fromWhere.y, toWhere.x, toWhere.y)
          }
          // TODO: Indicate empty spots.
      }

      renderedBubbles.clear()
      renderedBubbles ++= bubbles zip renderings map {
        case (b, r) => RenderedBubble(b, r.bounds)
      }
    }

    setLayout(null)
  }

  val messageLabel = new JLabel("Ready.")
  val canvas = new MainCanvas

  setLayout(new MigLayout((new LC).insets("0")))
  add(canvas, (new CC).cell(0, 0).grow().push())
  add(messageLabel, (new CC).cell(0, 1).growX())

  def message(text: String) {
    messageLabel setText text
  }

  var renderedBubbles = mutable.ArrayBuffer[RenderedBubble]()

  def bubbleAt(x: Int, y: Int): Option[Bubble] =
    (renderedBubbles filter (_.bounds contains (x, y))).headOption map (_.bubble)

  val reductionsPopup = new JLabel()
  reductionsPopup.setBorder(new TitledBorder("Reductions"))
  val reductionsPopupSig: Signal[Option[JComponent]] = focusedReductions map {
    case Some((bubble, focused)) if focused.length > 0 =>
      val rows = focused.toIndexedSeq.zipWithIndex map {
        case (reduction, i) =>
          <tr>
            <td>{if (i==0) "r" else ""}</td>
            <td>{(i+1).toString}</td>
            <td>{reduction.name}</td>
          </tr>
      }

      val html = <html><table>{rows}</table></html>

      canvas.add(reductionsPopup)

      reductionsPopup.setText(html.toString)
      reductionsPopup.setSize(reductionsPopup.getPreferredSize)
      reductionsPopup.setLocation(bubble.x + 40, bubble.y - reductionsPopup.getHeight/2)

      repaint()

      Some(reductionsPopup)
    case _ =>
      canvas.remove(reductionsPopup)
      repaint()
      None
  }
  reductionsPopupSig foreach { _ => } // Force evaluation.

  val popupPosition1 = reductionsPopupSig map { compy =>
    compy map { c =>
      c.getY + c.getHeight
    }
  }

  val buryPopup = new JPopupMenu()
  val buryPopupSig: Signal[Option[JComponent]] = buryChoices zip popupPosition1 map {
    case (Some((where, idioms)), prevPosition) if idioms.length > 0 =>
      buryPopup.removeAll()
      idioms foreach { idiom =>
        buryPopup.add(makeAction(idiom.name, addAntiPure(idiom, where)))
      }

      val x = where.x + 40
      val y = prevPosition map (_ + 10) getOrElse where.y
      buryPopup.show(canvas, x, y)

      Some(buryPopup)
    case _ =>
      buryPopup.setVisible(false)
      None
  }
  buryPopupSig foreach { _ => } // Force evaluation.

  val popupPosition2 = (buryPopupSig zip popupPosition1) map { case (compy, prev) =>
    compy map { c =>
      c.getY + c.getHeight
    } orElse prev
  }

  val recollectPopup = new JPopupMenu()
  val recollectPopupSig: Signal[Option[JComponent]] = recollectChoices zip popupPosition2 map {
    case (Some((where, idioms)), prevPosition) =>
      recollectPopup.removeAll()
      idioms foreach { idiom =>
        recollectPopup.add(makeAction(idiom.name, addPure(idiom,  where)))
      }

      val x = where.x + 40
      val y = prevPosition map (_ + 10) getOrElse where.y
      recollectPopup.show(canvas, x, y)

      Some(recollectPopup)
    case _ =>
      recollectPopup.setVisible(false)
      None
  }
  recollectPopupSig foreach { _ =>  }

  def makeAction(name: String, doIt: => Unit) = new AbstractAction() {
    putValue(Action.NAME, name)
    def actionPerformed(p1: ActionEvent) { doIt }
  }
}
