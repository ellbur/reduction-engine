
package reductionengine.gui

import java.awt._
import scala.App
import reductionengine.sugar

trait Rendering { this: Editor =>
  case class RenderedBubble(bubble: Bubble, bounds: Rectangle)
  case class BubbleRendering(bounds: Rectangle, parentAttachment: Point, childAttachments: Seq[Point])

  trait BasicTextBubbleRender { this: Bubble =>
    def text: String
    def bgColor: Color
    def fgColor: Color
    def arity: Int

    def render(g: Graphics2D) = {
      val bounds = renderTextBubble(g, haveFocus, text, x, y, bgColor=bgColor, fgColor=fgColor)
      BubbleRendering(bounds, new Point(bounds.x+bounds.width/2, bounds.y), 1 to arity map {
        i =>
          new Point(
            (i.toDouble / (1 + arity).toDouble * bounds.width + bounds.x).toInt,
            bounds.y + bounds.height
          )
      })
    }
  }

  trait ApicalOperatorRender { this: ApicalOperator =>
    def render(g: Graphics2D) = {
      op match {
        case sugar.B =>
          if (haveFocus) {
            g.setColor(new Color(0, 100, 0))
            g.setStroke(new BasicStroke(3))
          }
          else {
            g.setColor(Color.black)
            g.setStroke(new BasicStroke(1))
          }
          g.drawLine(x, y, x-4, y+4)
          g.drawLine(x, y, x+4, y+4)
          g.drawLine(x, y, x, y-4)

          BubbleRendering(
            new Rectangle(x-4, y-4, 8, 8),
            new Point(x, y-4),
            Seq(new Point(x-4, y+4), new Point(x+4, y+4))
          )
        case other =>
          val bounds = renderTextBubble(g, haveFocus, text, x, y, bgColor=bgColor, fgColor=fgColor)
          BubbleRendering(bounds, new Point(bounds.x+bounds.width/2, bounds.y), 1 to arity map {
            i =>
              new Point(
                (i.toDouble / (1 + arity).toDouble * bounds.width + bounds.x).toInt,
                bounds.y + bounds.height
              )
          })
      }
    }

    def text = op.name
    val bgColor = new Color(255, 230, 200)
    val fgColor = Color.black
    val arity = op.nArgs
  }

  trait IntLiteralRender extends BasicTextBubbleRender { this: IntLiteral =>
    def text = n.toString
    val bgColor = new Color(255, 255, 255)
    val fgColor = new Color(20, 100, 180)
    val arity = 0
  }

  trait RootRender { this: Root =>
    def render(g: Graphics2D) = {
      g.setColor(Color.yellow)
      g.fillRect(x-5, y-5, 5, 5)
      if (haveFocus) {
        g.setColor(Color.red)
        g.setStroke(new BasicStroke(2))
        g.drawRect(x-5, y-5, 5, 5)
      }
      BubbleRendering(new Rectangle(x-8, y-8, 8, 8), new Point(x, y), Seq(new Point(x, y)))
    }
  }

  trait NumberEditorRender { this: NumberEditor =>
    def render(g: Graphics2D) = {
      val x1 = editor.getX - 5
      val y1 = editor.getY - 5
      val width = editor.getWidth + 10
      val height = editor.getHeight + 10

      g.setColor(new Color(100, 100, 100))
      g.fillRoundRect(x1, y1, width, height, 5, 5)

      BubbleRendering(new Rectangle(x1, y1, width, height), new Point(x1+width/2, y1), Seq())
    }
  }

  trait MysteryRender extends BasicTextBubbleRender { this: Mystery =>
    val text = "?"
    val bgColor = new Color(220, 220, 220)
    val fgColor = Color.black
    val arity = 0
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

  def renderTextBubble(g: Graphics2D, focused: Boolean, text: String, x: Int, y: Int,
                       bgColor: Color = bubbleBGColor, fgColor: Color = textColor): Rectangle =
  {
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

    g.setColor(fgColor)
    g.drawString(text, x-width/2, y+height/2)

    new Rectangle(x1, y1, x2-x1, y2-y1)
  }
}
