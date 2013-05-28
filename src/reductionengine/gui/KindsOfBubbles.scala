
package reductionengine.gui

import java.awt.{Rectangle, BasicStroke, Color, Graphics2D}
import collection.mutable.ArrayBuffer
import javax.swing.JTextField
import java.awt.event.{ActionEvent, ActionListener}
import scalaz._
import Scalaz._
import reactive.{Observing, Signal}
import signalutils._
import redosignals._
import RedoSignals._

trait KindsOfBubbles { self: Editor =>
  import sugar.{ NewNode => NN, AlreadyThere => AT }
  import self.{sugar => s}
  import s.{LocalNode => l}
  import s.FocusedRNode

  trait BasicTextBubble { this: Bubble =>
    def text: String
    def focusedBGColor: Color
    def neutralBGColor: Color
    def fgColor: Color
    def arity: Int
    def round: Boolean = false

    def render(g: Graphics2D): BubbleRendering = {
      val Point(x, y) = location.now
      val bgColor =
        if (hasFocus)
          focusedBGColor
        else
          neutralBGColor
      val bounds = renderTextBubble(g, text, x, y, bgColor=bgColor, fgColor=fgColor, round=round)
      BubbleRendering(bounds, new Point(bounds.x+bounds.width/2, bounds.y), 1 to arity map {
        i =>
          new Point(
            (i.toDouble / (1 + arity).toDouble * bounds.width + bounds.x).toInt,
            bounds.y + bounds.height
          )
      })
    }
  }

  class IntLiteral(val initialLocation: Point, val n: Int) extends Bubble with BasicTextBubble
  {
    val initialChildren = Seq()
    lazy val localNode = l.IntLiteral(n).pure[Target]

    val text = n.toString
    val neutralBGColor = new Color(255, 255, 255)
    val focusedBGColor = new Color(150, 150, 150)
    val fgColor = new Color(20, 100, 180)
    val arity = 0
  }

  class ApicalOperator(val initialLocation: Point, op: s.SugarOperator, val initialChildren: Seq[Bubble]) extends Bubble {
    lazy val localNode = l.ApicalOperator(op).pure[Target]

    def render(g: Graphics2D) = {
      val Point(x, y) = location.now

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
          val bgColor =
            if (hasFocus)
              focusedBGColor
            else
              neutralBGColor
          val bounds = renderTextBubble(g, op.name, x, y, bgColor=bgColor, fgColor=fgColor, round=false)
          BubbleRendering(bounds, new Point(bounds.x+bounds.width/2, bounds.y), 1 to arity map {
            i =>
              new Point(
                (i.toDouble / (1 + arity).toDouble * bounds.width + bounds.x).toInt,
                bounds.y + bounds.height
              )
          })
      }
    }

    val neutralBGColor = new Color(255, 255, 255)
    val focusedBGColor = new Color(150, 150, 150)
    val fgColor = Color.black
    val arity = op.nArgs
  }

  class Pure(val initialLocation: Point, idiom: sugar.Idiom, initialIs: Bubble) extends Bubble with BasicTextBubble {
    val initialChildren = Seq(initialIs)
    lazy val localNode = l.Pure(idiom).pure[Target]

    def text =
      if (idiom.kind == sugar.standardIdiomKinds.lambda)
        s"λ${idiom.name}"
      else
        idiom.toString
    val neutralBGColor = new Color(255, 255, 255)
    val focusedBGColor = new Color(150, 150, 150)
    val fgColor = new Color(20, 20, 20)
    val arity = 1
  }

  class AntiPure(val initialLocation: Point, idiom: sugar.Idiom, initialIs: Bubble) extends Bubble with BasicTextBubble {
    val initialChildren = Seq(initialIs)
    lazy val localNode = l.AntiPure(idiom).pure[Target]

    def text = idiom.toString
    val neutralBGColor = new Color(255, 255, 255)
    val focusedBGColor = new Color(150, 150, 150)
    val fgColor = new Color(20, 20, 20)
    val arity = 1
  }

  class Mystery(val initialLocation: Point, name: String) extends Bubble with BasicTextBubble {
    val initialChildren = Seq()
    lazy val localNode = l.Mystery(name).pure[Target]

    val text = "?"
    val neutralBGColor = new Color(200, 200, 200)
    val focusedBGColor = new Color(150, 150, 150)
    val fgColor = Color.black
    val arity = 0
  }

  trait TextEditorBubble extends redosignals.Observing { this: Bubble =>
    val initialText: String
    val columns: Int = 6

    val editor = new JTextField(initialText, columns)
    editor.setSize(editor.getPreferredSize)
    location foreach { case Point(x, y) =>
      editor.setLocation(x-editor.getWidth/2, y-editor.getHeight/2)
    }

    def submit(text: String)

    editor.addActionListener(new ActionListener {
      def actionPerformed(ev: ActionEvent) {
        submit(editor.getText)
      }
    })

    editingState.focusedBubble foreach {
      case Some(bub) =>
        if (bub eq this)
          editor.requestFocus()
      case None =>
    }

    def render(g: Graphics2D): BubbleRendering = {
      val x1 = editor.getX - 5
      val y1 = editor.getY - 5
      val width = editor.getWidth + 10
      val height = editor.getHeight + 10

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

    val arity: Int

    override val components = Seq(editor)
  }

  class NumberEditor(val initialLocation: Point, name: String, val initialText: String) extends Bubble with TextEditorBubble {
    val initialChildren = Seq()
    lazy val localNode = l.NumberEditor(name, initialText).pure[Target]

    def submit(text: String) {
      try {
        val num = text.toInt
        val numNode = NN(s.IntLiteral(num))
        canvas.requestFocus()
        replace(this, FocusedRNode(numNode, Some(numNode)))
        actHard(s"Insert $num")
      }
      catch {
        case _: NumberFormatException =>
          message(s"$text is not a number")
      }
    }

    val arity = 0
  }

  class AntiPureNameEditor(val initialLocation: Point, idiomKind: sugar.KindOfIdiom, val initialText: String, initialOf: Bubble)
    extends Bubble with TextEditorBubble
  {
    val initialChildren = Seq(initialOf)
    val of = children map {
      case Seq(of) => of
    }
    lazy val localNode = l.AntiPureNameEditor(idiomKind, initialText).pure[Target]

    def submit(text: String) {
      val idiom = sugar.Idiom(idiomKind, text)
      val apNode = NN(s.AntiPure(idiom, AT(of.now)))
      canvas.requestFocus()
      replace(this, FocusedRNode(apNode, Some(apNode)))
      actHard(s"Collect ${idiomKind.name} $text")
    }

    val arity = 1
  }

  class PureNameEditor(val initialLocation: Point, idiomKind: sugar.KindOfIdiom, val initialText: String, initialOf: Bubble)
    extends Bubble with TextEditorBubble
  {
    val initialChildren = Seq(initialOf)
    val of = children map {
      case Seq(of) => of
    }
    lazy val localNode = l.PureNameEditor(idiomKind, initialText).pure[Target]

    def submit(text: String) {
      val idiom = sugar.Idiom(idiomKind, text)
      val pNode = NN(s.Pure(idiom, AT(of.now)))
      canvas.requestFocus()
      replace(this, FocusedRNode(pNode, Some(pNode)))
      actHard(s"Bury ${idiomKind.name} $text")
    }

    val arity = 1
  }

  class VariableEditor(val initialLocation: Point, name: String, val initialText: String) extends Bubble with TextEditorBubble {
    val initialChildren = Seq()
    lazy val localNode = l.VariableEditor(name, initialText).pure[Target]

    def submit(text: String) {
      val name = text
      val node = NN(s.Variable(name))
      canvas.requestFocus()
      replace(this, FocusedRNode(node, Some(node)))
      actHard(s"Variable $name")
    }

    val arity = 0
  }

  class Variable(val initialLocation: Point, name: String) extends Bubble with BasicTextBubble {
    val initialChildren = Seq()
    lazy val localNode = l.Variable(name).pure[Target]

    val text = name
    val neutralBGColor = new Color(255, 255, 255)
    val focusedBGColor = new Color(150, 150, 150)
    val fgColor = Color.black
    val arity = 0
  }

  val bubbleLineColor = Color.black
  val bubbleFocusedLineColor = new Color(0, 80, 0)

  val bubblePad = 2

  def renderTextBubble(g: Graphics2D, text: String, x: Int, y: Int,
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

    g.setColor(fgColor)
    g.drawString(text, x-width/2, y+height/2)

    new Rectangle(x1, y1, x2-x1, y2-y1)
  }
}
