
package reductionengine.gui

import java.awt.Graphics2D
import reductionengine.{logic, sugar}
import collection.mutable.ArrayBuffer
import collection.mutable
import javax.swing.JTextField
import java.awt.event.{ActionEvent, ActionListener}

trait KindsOfBubbles { this: Editor =>
  class IntLiteral(
                         var n: Int,
                         var x: Int,
                         var y: Int) extends Bubble with IntLiteralRender
  {
    def childEdges = Seq()
    def toSugarNode = sugar.IntLiteral(n)
  }
  object IntLiteral {
    def apply(n: Int, x: Int, y: Int) = new IntLiteral(n, x, y)
  }

  class ApicalOperator(var op: sugar.SugarOperator, var childBubbles: mutable.ArrayBuffer[Bubble], var x: Int, var y: Int)
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
              focusedBubble = Some(thisBubble)
              focusedParent = identifyAParent(thisBubble)
              focusedChild = identifyAChild(thisBubble)
            }
            else {
              focusedBubble = Some(childBubbles(i - 1))
              focusedParent = Some(thisBubble)
              focusedChild = identifyAChild(childBubbles(i - 1))
            }
          }
          else {
            // TODO
            message("Deleting inner parameters is not implemented.")
          }
        }
      }
    }
    def toSugarNode = sugar.ApicalOperator(op, childBubbles.toSeq)
  }
  object ApicalOperator {
    def apply(op: sugar.SugarOperator, childBubbles: Seq[Bubble], x: Int, y: Int) =
      new ApicalOperator(op, mutable.ArrayBuffer(childBubbles: _*), x, y)
  }

  class Root(var is: Bubble, var x: Int, var y: Int) extends Bubble with RootRender
  {
    def childEdges = Seq(basicEdge(is, is = _))
    def toSugarNode = sugar.Root(is)
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

  class NumberEditor(id: Int, initially: String, var x: Int, var y: Int) extends Bubble with NumberEditorRender { thisBubble =>
    val editor = new JTextField(initially, 6)

    override val components = Seq(editor)

    def go() {
      editor.setLocation(x + 5, y + 5)
      editor.setSize(editor.getPreferredSize)
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
        try {
          replace(thisBubble, sugar.NewNode(sugar.Focused(sugar.IntLiteral(editor.getText.toInt))))
        }
        catch {
          case _: NumberFormatException =>
            message("Not a number: " + editor.getText)
        }
      }
    })

    def childEdges = Seq()
    def toSugarNode = sugar.NumberEditor(id, editor.getText)
  }
  object NumberEditor {
    def apply(id: Int, initially: String, x: Int, y: Int) = new NumberEditor(id, initially, x, y)
  }
}
