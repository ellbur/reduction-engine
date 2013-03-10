
import collection.mutable.ArrayBuffer
import java.awt.{Color, Graphics2D, Graphics}
import javax.swing.{JPanel, JLabel, JFrame}

object Main {
  def main(args: Array[String]) {
    val win = new JFrame()
    win.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    win.add(new Pasture)

    win.setSize(800, 600)
    win.setVisible(true)
  }

  class Pasture extends JPanel {
    val xPad = 5
    val yPad = 5

    setBackground(Color.white)
    val edgeLineColor = Color.black
    val bubbleBGColor = new Color(200, 200, 255)
    val bubbleLineColor = Color.black
    val textColor = Color.black

    override def paintComponent(_g: Graphics) {
      val g = _g.asInstanceOf[Graphics2D]

      val fm = g.getFontMetrics

      g.setColor(getBackground)
      g.fillRect(0, 0, getWidth, getHeight)

      val bubbleCenters = Map(bubbles map { b =>
        val width = fm.stringWidth(b.text)
        val height = fm.getHeight

        (b, (b.x + width/2, b.y - height/2))
      }: _*)

      g.setColor(edgeLineColor)
      for (e <- edges) {
        val (x1, y1) = bubbleCenters(e.b1)
        val (x2, y2) = bubbleCenters(e.b2)
        g.drawLine(x1, y1, x2, y2)
      }

      for (b <- bubbles) {
        val width = fm.stringWidth(b.text)
        val height = fm.getHeight

        val x1 = b.x - xPad
        val x2 = x1 + width + 2*xPad
        val y1 = b.y + yPad
        val y2 = y1 - height - 2*yPad

        g.setColor(bubbleBGColor)
        g.fillRoundRect(x1, y2, x2-x1, y1-y2, 3, 3)

        g.setColor(bubbleLineColor)
        g.drawRoundRect(x1, y2, x2-x1, y1-y2, 3, 3)

        g.setColor(textColor)
        g.drawString(b.text, b.x, b.y)
      }
    }

    val bubbles = ArrayBuffer[Bubble](
      new Bubble(400, 100, "+"),
      new Bubble(300, 200, "3"),
      new Bubble(500, 200, "4")
    )

    val edges = ArrayBuffer[Edge](
      new Edge(bubbles(0), bubbles(1)),
      new Edge(bubbles(0), bubbles(2))
    )

    class Bubble(
      var x: Int,
      var y: Int,
      var text: String)

    class Edge(
      var b1: Bubble,
      var b2: Bubble)
  }
}
