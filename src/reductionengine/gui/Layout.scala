
package reductionengine.gui

import javax.swing.{JLabel, JPanel}
import java.awt.{Color, Graphics2D, Graphics}
import net.miginfocom.swing.MigLayout
import net.miginfocom.layout.{CC, LC}

trait Layout { this: Editor =>
  class MainCanvas extends JPanel with SetupKeys {
    override def paintComponent(_g: Graphics) {
      val g = _g.asInstanceOf[Graphics2D]

      g.setColor(Color.white)
      g.fillRect(0, 0, getSize().width, getSize().height)

      for (bc <- bubbles)
        bc.bubble.renderEdges(g)

      for (bc <- bubbles)
        bc.bubble.render(g)

      val focused = focusedReductions.toSeq
      if (focused.length > 0) {
        val rows = focused.toIndexedSeq.zipWithIndex map {
          case (reduction, i) =>
            <tr>
              <td>{if (i==0) "r" else ""}</td>
              <td>{(i+1).toString}</td>
              <td>{reduction.description}</td>
            </tr>
        }

        val html = <html><table>{rows}</table></html>

        val label = new JLabel(html.toString)
        label.setSize(label.getPreferredSize)
        val tr = g.getTransform
        val where = focusedBubble match {
          case Some(bubble) => (bubble.bubble.x + 40, bubble.bubble.y - label.getHeight/2)
          case None => (20, label.getHeight + 20)
        }
        g.translate(where._1, where._2)
        label.paint(g)
        g.setTransform(tr)
      }
    }
  }

  val messageLabel = new JLabel("Ready.")

  setLayout(new MigLayout((new LC).insets("0")))
  add(new MainCanvas, (new CC).cell(0, 0).grow().push())
  add(messageLabel, (new CC).cell(0, 1).growX())

  def message(text: String) {
    messageLabel setText text
  }
}
