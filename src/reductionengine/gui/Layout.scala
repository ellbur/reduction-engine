
package reductionengine.gui

import javax.swing.{JLabel, JPanel}
import java.awt.{BasicStroke, Color, Graphics2D, Graphics}
import net.miginfocom.swing.MigLayout
import net.miginfocom.layout.{CC, LC}
import scala.collection.mutable

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

      val focused = focusedReductions.toSeq
      if (focused.length > 0) {
        val rows = focused.toIndexedSeq.zipWithIndex map {
          case (reduction, i) =>
            <tr>
              <td>{if (i==0) "r" else ""}</td>
              <td>{(i+1).toString}</td>
              <td>{reduction.name}</td>
            </tr>
        }

        val html = <html><table>{rows}</table></html>

        val label = new JLabel(html.toString)
        label.setSize(label.getPreferredSize)
        val tr = g.getTransform
        val where = focusedBubble match {
          case Some(bubble) => (bubble.x + 40, bubble.y - label.getHeight/2)
          case None => (20, label.getHeight + 20)
        }
        g.translate(where._1, where._2)
        label.paint(g)
        g.setTransform(tr)
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
}
