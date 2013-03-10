
package reductionengine.gui

import java.awt.{BasicStroke, Color, Graphics2D}
import scala.App

trait Rendering { this: Editor =>
  trait AppRender { this: App =>
    def render(g: Graphics2D) {
      if (haveFocus) {
        g.setColor(new Color(0, 100, 0))
        g.fillOval(x-4, y-4, 8, 8)
      }
      else {
        g.setColor(Color.black)
        g.fillOval(x-2, y-2, 4, 4)
      }
    }

    def renderEdges(g: Graphics2D) {
      connectLine(g, this, car.bubble)
      connectLine(g, this, cdr.bubble)
    }
  }

  trait IntLiteralRender { this: IntLiteral =>
    def render(g: Graphics2D) {
      renderTextBubble(g, haveFocus, n.toString, x, y)
    }

    def renderEdges(g: Graphics2D) { }
  }

  trait PlusRender { this: Plus =>
    def render(g: Graphics2D) {
      renderTextBubble(g, haveFocus, "+", x, y)
    }

    def renderEdges(g: Graphics2D) { }
  }

  trait RootRender { this: Root =>
    def render(g: Graphics2D) {
      g.setColor(Color.yellow)
      g.fillRect(x-5, y-5, 5, 5)
      if (haveFocus) {
        g.setColor(Color.red)
        g.setStroke(new BasicStroke(2))
        g.drawRect(x-5, y-5, 5, 5)
      }
    }

    def renderEdges(g: Graphics2D) {
      connectLine(g, this, is.bubble)
    }
  }

  trait MysteryRender { this: Mystery =>
    def render(g: Graphics2D) {
      renderTextBubble(g, haveFocus, "?", x, y, bgColor=new Color(200, 200, 200))
    }

    def renderEdges(g: Graphics2D) { }
  }

  val bubbleBGColor = new Color(230, 240, 255)
  val bubbleLineColor = Color.black
  val bubbleFocusedLineColor = new Color(0, 80, 0)
  val textColor = Color.black

  val bubblePad = 5

  def connectLine(g: Graphics2D, from: Bubble, to: Bubble) {
    if (from.isFocusedParent && to.hasFocus) {
      g.setColor(new Color(100, 200, 100))
      g.setStroke(new BasicStroke(2))
      g.drawLine(from.x, from.y, to.x, to.y)
    }
    else if (to.isFocusedChild && from.hasFocus) {
      g.setColor(new Color(150, 200, 150))
      g.setStroke(new BasicStroke(2))
      g.drawLine(from.x, from.y, to.x, to.y)
    }
    else {
      g.setColor(new Color(0, 0, 0))
      g.setStroke(new BasicStroke(1))
      g.drawLine(from.x, from.y, to.x, to.y)
    }
  }

  def renderTextBubble(g: Graphics2D, focused: Boolean, text: String, x: Int, y: Int, bgColor: Color = bubbleBGColor) {
    val fm = g.getFontMetrics
    val pad = bubblePad

    val width = fm.stringWidth(text)
    val height = fm.getHeight

    val x1 = x - width/2 - pad
    val x2 = x + width/2 + pad
    val y1 = y - height/2 - pad
    val y2 = y + height/2 + pad

    g.setColor(bgColor)
    g.fillRoundRect(x1, y1, x2-x1, y2-y1, 5, 5)

    if (focused) {
      g.setColor(bubbleFocusedLineColor)
      g.setStroke(new BasicStroke(2))
      g.drawRoundRect(x1, y1, x2-x1, y2-y1, 5, 5)
    }
    else {
      g.setColor(bubbleLineColor)
      g.setStroke(new BasicStroke(1))
      g.drawRoundRect(x1, y1, x2-x1, y2-y1, 5, 5)
    }

    g.setColor(textColor)
    g.drawString(text, x-width/2, y+height/2)
  }
}
