
package reductionengine.gui

import javax.swing.{JComponent, AbstractAction, KeyStroke}
import java.awt.event._

trait Inputs { self: Editor =>
  import sugar.logic
  import self.{sugar => s}

  trait SetupInputs { this: JComponent =>
    setFocusable(true)

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

      var pressingGroup = Option[DraggedGroup](null)

      bind(VK_UP, SHIFT_DOWN_MASK, moveCurrentGroupUp())
      bind(VK_DOWN, SHIFT_DOWN_MASK, moveCurrentGroupDown())
      bind(VK_LEFT, SHIFT_DOWN_MASK, moveCurrentGroupLeft())
      bind(VK_RIGHT, SHIFT_DOWN_MASK, moveCurrentGroupRight())

      bind(VK_UP, 0, moveToParent())
      bind(VK_DOWN, 0, moveToChild())
      bind(VK_LEFT, 0, moveToLeftBranch())
      bind(VK_RIGHT, 0, moveToRightBranch())

      bind(VK_TAB, 0, moveToNextHole())
      bind(VK_TAB, SHIFT_DOWN_MASK, moveToPreviousHole())

      bind('r', reduceCurrentNode())

      bind('+', doOperator(s.BasicOperator(logic.Plus)))
      bind('-', doOperator(s.BasicOperator(logic.Minus)))
      bind('*', doOperator(s.BasicOperator(logic.Times)))

      bind('S', doOperator(s.BasicOperator(logic.S(1))))
      bind('K', doOperator(s.BasicOperator(logic.K(1, Seq(false)))))
      bind('I', doOperator(s.BasicOperator(logic.I)))
      bind('Y', doOperator(s.BasicOperator(logic.Y)))

      bind('.', doOperator(s.B))

      bind('e', stripMystery())
      bind('C', clearToMystery())
      bind('i', insertChild())

      bind('f', reformatSubtree())

      bind('b', showBuryMenu())
      bind('c', showRecollectMenu())

      for (k <- '0' to '9') bind(k, beginTypingNumber(Some(k)))
      bind('#', beginTypingNumber(None))

      val mouse = new MouseListener with MouseMotionListener {
        def mouseExited(p1: MouseEvent) {}
        def mouseClicked(p1: MouseEvent) {}
        def mouseEntered(p1: MouseEvent) {}
        def mousePressed(ev: MouseEvent) {
          requestFocus()

          if (ev.isControlDown) {
            makeNewRootAtPoint(ev.getX, ev.getY)
          }
          else {
            val here = bubbleAt(ev.getX, ev.getY)
            focusedBubble() = here
            pressingGroup = here map { bubble =>
              DraggedGroup(computeGroup(bubble), ev.getX, ev.getY)
            }
          }

          repaint()
          updateBubblyThings()
        }
        def mouseReleased(p1: MouseEvent) {}
        def mouseDragged(ev: MouseEvent) {
          pressingGroup foreach { case DraggedGroup(group, x, y) =>
            moveGroup(group, ev.getX - x, ev.getY - y)
            pressingGroup = Some(DraggedGroup(group, ev.getX, ev.getY))
            repaint()
          }
        }
        def mouseMoved(ev: MouseEvent) { }
      }
      addMouseListener(mouse)
      addMouseMotionListener(mouse)
    }
  }

  case class DraggedGroup(members: Traversable[Bubble], x: Int, y: Int)
}
