
package reductionengine.gui

import java.awt._
import event.{ActionEvent, ActionListener}
import scala.App
import reductionengine.sugar
import javax.swing.{JTextField, JComponent}
import reactive.Observing

trait Rendering { this: Editor =>
  case class RenderedBubble(bubble: Bubble, bounds: Rectangle)
  case class BubbleRendering(bounds: Rectangle, parentAttachment: Point, childAttachments: Seq[Point])

  trait BasicEditing extends DefaultEditing {
    def text: String
    def bgColor: Color
    def fgColor: Color
    def arity: Int
    def round: Boolean = false

    def render(g: Graphics2D, hasFocus: Boolean): BubbleRendering = {
      val (x, y) = (this.x.now, this.y.now)
      val bounds = renderTextBubble(g, hasFocus, text, x, y, bgColor=bgColor, fgColor=fgColor, round=round)
      BubbleRendering(bounds, new Point(bounds.x+bounds.width/2, bounds.y), 1 to arity map {
        i =>
          new Point(
            (i.toDouble / (1 + arity).toDouble * bounds.width + bounds.x).toInt,
            bounds.y + bounds.height
          )
      })
    }
  }

  class IntLiteralEditing(val xInit: Int, val yInit: Int, val n: Int) extends BasicEditing {
    val text = n.toString
    val bgColor = new Color(255, 255, 255)
    val fgColor = new Color(20, 100, 180)
    val arity = 0
  }

  class ApicalOperatorEditing(val xInit: Int, val yInit: Int, val op: sugar.SugarOperator) extends DefaultEditing {
    def render(g: Graphics2D, hasFocus: Boolean) = {
      val (x, y) = (this.x.now, this.y.now)

      op match {
        case sugar.B =>
          if (hasFocus) {
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
          val bounds = renderTextBubble(g, hasFocus, op.name, x, y, bgColor=bgColor, fgColor=fgColor, round=false)
          BubbleRendering(bounds, new Point(bounds.x+bounds.width/2, bounds.y), 1 to arity map {
            i =>
              new Point(
                (i.toDouble / (1 + arity).toDouble * bounds.width + bounds.x).toInt,
                bounds.y + bounds.height
              )
          })
      }
    }

    val bgColor = new Color(255, 230, 200)
    val fgColor = Color.black
    val arity = op.nArgs
  }

  class PureEditing(val xInit: Int, val yInit: Int, val idiom: sugar.Idiom) extends BasicEditing {
    def text = idiom.toString
    val bgColor = new Color(200, 190, 255)
    val fgColor = new Color(20, 20, 20)
    val arity = 1
  }

  class AntiPureEditing(val xInit: Int, val yInit: Int, val idiom: sugar.Idiom) extends BasicEditing {
    def text = idiom.toString
    val bgColor = new Color(200, 255, 190)
    val fgColor = new Color(20, 20, 20)
    val arity = 1
  }

  class RootEditing(val xInit: Int, val yInit: Int) extends DefaultEditing {
    def render(g: Graphics2D, hasFocus: Boolean) = {
      val (x, y) = (this.x.now, this.y.now)
      g.setColor(Color.yellow)
      g.fillRect(x-5, y-5, 5, 5)
      if (hasFocus) {
        g.setColor(Color.red)
        g.setStroke(new BasicStroke(2))
        g.drawRect(x-5, y-5, 5, 5)
      }
      BubbleRendering(new Rectangle(x-8, y-8, 8, 8), new Point(x, y), Seq(new Point(x, y)))
    }
  }

  trait TextEditorEditing extends DefaultEditing with Observing {
    val initialText: String
    val arity: Int
    def submit(text: String)

    val editor = new JTextField(initialText, 6)
    canvas.add(editor)
    editor.setSize(editor.getPreferredSize)
    editor.requestFocus()

    editor.addActionListener(new ActionListener {
      def actionPerformed(ev: ActionEvent) {
        submit(editor.getText)
      }
    })

    (x zip y) foreach {
      case (x, y) =>
        editor.setLocation(x - editor.getWidth/2, y - editor.getHeight/2)
    }

    editingState.visibleBubbles.toMap.change foreach { map =>
      map map (_._2) exists(_ == this) match {
        case false =>
          canvas.remove(editor)
        case true =>
          // oK
      }
    }

    def render(g: Graphics2D, hasFocus: Boolean) = {
      val x1 = editor.getX - 5
      val y1 = editor.getY - 5
      val width = editor.getWidth + 10
      val height = editor.getHeight + 10

      g.setColor(bgColor)
      g.fillRoundRect(x1, y1, width, height, 5, 5)

      BubbleRendering(new Rectangle(x1, y1, width, height), new Point(x1+width/2, y1),
        1 to arity map {
          i =>
            new Point(
              (i.toDouble / (1 + arity).toDouble * width + x1).toInt,
              y1 + height
            )
        }
      )
    }

    val bgColor: Color
  }

  class NumberEditorEditing(val xInit: Int, val yInit: Int, val initialText: String) extends TextEditorEditing {
    val bgColor = new Color(100, 100, 100)
    val arity = 0

    def submit(text: String) {
      (try Some(text.toInt) catch { case _: NumberFormatException => None }) match {
        case Some(n) =>
          editingState.bubbleFor(this) foreach { bubble =>
            replace(bubble, sugar.NewNode(sugar.IntLiteral(n)))
            canvas.requestFocus()
            actHard(s"Insert $text")
          }
        case None =>
          message(s"$text is not a number.")
      }
    }
  }

  class AntiPureNameEditorEditing(val xInit: Int, val yInit: Int, val idiomKind: sugar.KindOfIdiom, val initialText: String) extends TextEditorEditing {
    val bgColor = new Color(0, 0, 100)
    val arity = 1
    def submit(text: String) {
      // This is obviously really messed up.
      editingState.bubbleFor(this) foreach {
        case bubble: AntiPureNameEditor =>
          val idiom = sugar.Idiom(idiomKind, text)
          replace(bubble, sugar.NewNode(sugar.AntiPure(idiom, sugar.AlreadyThere(bubble.of))))
          canvas.requestFocus()
          actHard(s"Bury $idiomKind($text)")
        case _ =>
      }
    }
  }

  class PureNameEditorEditing(val xInit: Int, val yInit: Int, val idiomKind: sugar.KindOfIdiom, val initialText: String) extends TextEditorEditing {
    val bgColor = new Color(100, 0, 0)
    val arity = 1
    def submit(text: String) {
      editingState.bubbleFor(this) foreach {
        case bubble: PureNameEditor =>
          val idiom = sugar.Idiom(idiomKind, text)
          replace(bubble, sugar.NewNode(sugar.Pure(idiom, sugar.AlreadyThere(bubble.of))))
          canvas.requestFocus()
          actHard(s"Recollect $idiomKind($text)")
        case _ =>
      }
    }
  }

  class VariableEditorEditing(val xInit: Int, val yInit: Int, val initialText: String) extends TextEditorEditing {
    val bgColor = new Color(200, 200, 255)
    val arity = 0
    def submit(text: String) {
      editingState.bubbleFor(this) foreach {
        case bubble: VariableEditor =>
          replace(bubble, sugar.NewNode(sugar.Variable(text)))
          canvas.requestFocus()
          actHard(s"Insert variable $text")
        case _ =>
      }
    }
  }

  class VariableEditing(val xInit: Int, val yInit: Int, name: String) extends BasicEditing {
    val text = name
    val bgColor = new Color(200, 200, 255)
    val fgColor = new Color(0, 0, 0)
    val arity = 0
    override val round = true
  }

  class MysteryEditing(val xInit: Int, val yInit: Int) extends BasicEditing {
    val text = "?"
    val bgColor = new Color(220, 220, 220)
    val fgColor = Color.black
    val arity = 0
  }

  val bubbleLineColor = Color.black
  val bubbleFocusedLineColor = new Color(0, 80, 0)

  val bubblePad = 5

  def renderTextBubble(g: Graphics2D, focused: Boolean, text: String, x: Int, y: Int,
                       bgColor: Color, fgColor: Color, round: Boolean): Rectangle =
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
    if (round)
      g.fillOval(x1, y1, x2-x1, y2-y1)
    else
      g.fillRoundRect(x1, y1, x2-x1, y2-y1, 5, 5)

    if (focused) {
      g.setColor(bubbleFocusedLineColor)
      g.setStroke(new BasicStroke(2))
    }
    else {
      g.setColor(bubbleLineColor)
      g.setStroke(new BasicStroke(1))
    }
    if (round)
      g.drawOval(x1, y1, x2-x1, y2-y1)
    else
      g.drawRoundRect(x1, y1, x2-x1, y2-y1, 5, 5)

    g.setColor(fgColor)
    g.drawString(text, x-width/2, y+height/2)

    new Rectangle(x1, y1, x2-x1, y2-y1)
  }
}
