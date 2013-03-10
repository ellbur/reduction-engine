
package reductionengine.gui

import javax.swing.{JComponent, AbstractAction, KeyStroke}
import java.awt.event._

trait Keys { this: Editor =>
  trait SetupKeys { this: JComponent =>
    override def isFocusable = true

    var actionCounter: Int = 0

    def bind(ks: KeyStroke, action: => Unit) {
      val id = "action" + actionCounter.toString

      getInputMap.put(ks, id)
      getActionMap.put(id, new AbstractAction {
        def actionPerformed(ev: ActionEvent) {
          action
          repaint()
        }
      })

      actionCounter += 1
    }

    def bind(code: Int, modifiers: Int, action: => Unit) {
      bind(KeyStroke.getKeyStroke(code, modifiers), action)
    }

    def bind(code: Char, action: => Unit) {
      bind(KeyStroke.getKeyStroke(code), action)
    }

    // Key bindings

    {
      import KeyEvent._
      import InputEvent._

      bind(VK_UP, SHIFT_DOWN_MASK, moveCurrentGroupUp())
      bind(VK_DOWN, SHIFT_DOWN_MASK, moveCurrentGroupDown())
      bind(VK_LEFT, SHIFT_DOWN_MASK, moveCurrentGroupLeft())
      bind(VK_RIGHT, SHIFT_DOWN_MASK, moveCurrentGroupRight())

      bind(VK_UP, 0, moveToParent())
      bind(VK_DOWN, 0, moveToChild())
      bind(VK_LEFT, 0, moveToLeftBranch())
      bind(VK_RIGHT, 0, moveToRightBranch())

      bind('r', reduceCurrentNode())

      bind('+', doPlus())

      addMouseListener(new MouseListener {
        def mouseExited(p1: MouseEvent) {}
        def mouseClicked(p1: MouseEvent) {}
        def mouseEntered(p1: MouseEvent) {}
        def mousePressed(ev: MouseEvent) {
          // TODO: Determine if we are on a bubble.
          if (ev.isControlDown) {
            makeNewRootAtPoint(ev.getX, ev.getY)
            repaint()
          }
        }
        def mouseReleased(p1: MouseEvent) {}
      })
    }
  }
}
