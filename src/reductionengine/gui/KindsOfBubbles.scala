
package reductionengine.gui

import java.awt.Graphics2D
import reductionengine.{logic, sugar}
import collection.mutable.ArrayBuffer
import collection.mutable
import javax.swing.JTextField
import java.awt.event.{ActionEvent, ActionListener}

trait KindsOfBubbles { this: Editor =>
  import sugar.{ NewNode => NN, AlreadyThere => AT }

  class IntLiteral(
                         var n: Int,
                         var x: Int,
                         var y: Int) extends Bubble with IntLiteralRender
  {
    def childEdges = Seq()
    def toSugarNode = sugar.IntLiteral(n)
    override def toString = n.toString
  }
  object IntLiteral {
    def apply(n: Int, x: Int, y: Int) = new IntLiteral(n, x, y)
  }

  class ApicalOperator(var op: sugar.SugarOperator, val childBubbles: ArrayBuffer[Bubble], var x: Int, var y: Int)
    extends Bubble with ApicalOperatorRender
  { thisBubble =>
    def childEdges = childBubbles.indices map { i =>
      new Edge {
        def target = childBubbles(i)
        def substitute(`with`: Bubble) { childBubbles(i) = `with` }
        override def deleteIfReasonable() {
          if (i == childBubbles.length-1) {
            childBubbles.remove(i)
            if (childBubbles.length == 0) {
              focusedBubble() = Some(thisBubble)
              focusedParent() = identifyAParent(thisBubble)
              focusedChild() = identifyAChild(thisBubble)
            }
            else {
              focusedBubble() = Some(childBubbles(i - 1))
              focusedParent() = Some(thisBubble)
              focusedChild() = identifyAChild(childBubbles(i - 1))
            }
          }
          else {
            // TODO
            message("Deleting inner parameters is not implemented.")
          }
        }
      }
    }
    def toSugarNode = sugar.ApicalOperator(op, (childBubbles map (sugar.AlreadyThere(_))).toSeq)
  }
  object ApicalOperator {
    def apply(op: sugar.SugarOperator, childBubbles: Seq[Bubble], x: Int, y: Int) =
      new ApicalOperator(op, ArrayBuffer(childBubbles: _*), x, y)
  }

  class Pure(var idiom: sugar.Idiom, var is: Bubble, var x: Int, var y: Int) extends Bubble with PureRender {
    def childEdges = Seq(basicEdge(is, is = _))
    def toSugarNode = sugar.Pure(idiom, sugar.AlreadyThere(is))
  }

  object Pure {
    def apply(idiom: sugar.Idiom, is: Bubble, x: Int, y: Int) = new Pure(idiom, is, x, y)
  }

  class AntiPure(var idiom: sugar.Idiom, var is: Bubble, var x: Int, var y: Int) extends Bubble with AntiPureRender {
    def childEdges = Seq(basicEdge(is, is = _))
    def toSugarNode = sugar.AntiPure(idiom, sugar.AlreadyThere(is))
  }

  object AntiPure {
    def apply(idiom: sugar.Idiom, is: Bubble, x: Int, y: Int) = new AntiPure(idiom, is, x, y)
  }

  class Root(var is: Bubble, var x: Int, var y: Int) extends Bubble with RootRender
  {
    def childEdges = Seq(basicEdge(is, is = _))
    def toSugarNode = sugar.Root(sugar.AlreadyThere(is))
  }
  object Root {
    def apply(is: Bubble, x: Int, y: Int) = new Root(is, x, y)
  }

  class Mystery(var n: Int, var x: Int, var y: Int) extends Bubble with MysteryRender
  {
    def childEdges = Seq()
    def toSugarNode = sugar.Mystery(n)
  }
  object Mystery {
    def apply(n: Int, x: Int, y: Int) = new Mystery(n, x, y)
  }

  trait TextEditorBubble { thisBubble: Bubble =>
    val initialText: String

    val editor = new JTextField(initialText, 6)

    override val components = Seq(editor)

    def go() {
      editor.setSize(editor.getPreferredSize)
      editor.setLocation(x-editor.getWidth/2, y-editor.getHeight/2)
    }
    go()
    override def move(dx: Int, dy: Int) {
      x += dx
      y += dy
      go()
    }

    override def receiveFocus() { editor.requestFocus() }

    editor.addActionListener(new ActionListener {
      def actionPerformed(p1: ActionEvent) {
        thisBubble.actionPerformed(p1)
      }
    })

    def actionPerformed(ev: ActionEvent): Unit
  }

  class NumberEditor(id: Int, initially: String, var x: Int, var y: Int)
    extends Bubble with TextEditorBubble with NumberEditorRender
  {
    lazy val initialText = initially

    def actionPerformed(ev: ActionEvent) {
      try {
        replace(this, sugar.NewNode(sugar.Focused(NN(sugar.IntLiteral(editor.getText.toInt)))))
        repaint()
      }
      catch {
        case _: NumberFormatException =>
          message("Not a number: " + editor.getText)
      }
    }

    def childEdges = Seq()
    def toSugarNode = sugar.NumberEditor(id, editor.getText)
  }
  object NumberEditor {
    def apply(id: Int, initially: String, x: Int, y: Int) = new NumberEditor(id, initially, x, y)
  }

  class AntiPureNameEditor(idiomKind: sugar.KindOfIdiom, initially: String, var of: Bubble, var x: Int, var y: Int)
    extends Bubble with TextEditorBubble with AntiPureNameEditorRender
  {
    lazy val initialText = initially
    def childEdges = Seq(basicEdge(of, of = _))
    def toSugarNode = sugar.AntiPureNameEditor(idiomKind, editor.getText, AT(of))

    def actionPerformed(ev: ActionEvent) {
      replace(this, NN(sugar.Focused(
        NN(sugar.AntiPure(sugar.Idiom(idiomKind, editor.getText), AT(of)))
      )))
    }
  }
  object AntiPureNameEditor {
    def apply(idiomKind: sugar.KindOfIdiom, initially: String, of: Bubble, x: Int, y: Int) =
      new AntiPureNameEditor(idiomKind, initially, of, x, y)
  }

  class PureNameEditor(idiomKind: sugar.KindOfIdiom, initially: String, var of: Bubble, var x: Int, var y: Int)
    extends Bubble with TextEditorBubble with PureNameEditorRender
  {
    lazy val initialText = initially
    def childEdges = Seq(basicEdge(of, of = _))
    def toSugarNode = sugar.PureNameEditor(idiomKind, editor.getText, AT(of))

    def actionPerformed(ev: ActionEvent) {
      replace(this, NN(sugar.Focused(
        NN(sugar.Pure(sugar.Idiom(idiomKind, editor.getText), AT(of)))
      )))
    }
  }
  object PureNameEditor {
    def apply(idiomKind: sugar.KindOfIdiom, initially: String, of: Bubble, x: Int, y: Int) =
      new PureNameEditor(idiomKind, initially, of, x, y)
  }
}
