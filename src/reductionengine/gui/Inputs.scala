
package reductionengine.gui

import javax.swing._
import java.awt.event._

trait Inputs { self: Editor =>
  import sugar.logic
  import self.{sugar => s}

  trait SetupInputs { this: JComponent =>
    setFocusable(true)
    setFocusTraversalKeysEnabled(false)

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

    {
      import KeyEvent._
      import InputEvent._

      object ways {
        def k(c: Char) = KeyStroke.getKeyStroke(c)
        def k(c: Int, mods: Int) = KeyStroke.getKeyStroke(c, mods)

        def i(s: KeyStroke, action: => Unit)  {
          bind(s, action)
        }
      }
      import ways._

      i(k(VK_UP, SHIFT_DOWN_MASK), moveCurrentGroupUp())
      i(k(VK_DOWN, SHIFT_DOWN_MASK), moveCurrentGroupDown())
      i(k(VK_LEFT, SHIFT_DOWN_MASK), moveCurrentGroupLeft())
      i(k(VK_RIGHT, SHIFT_DOWN_MASK), moveCurrentGroupRight())

      i(k(VK_UP, 0), moveToParent())
      i(k(VK_DOWN, 0), moveToChild())
      i(k(VK_LEFT, 0), moveToLeftBranch())
      i(k(VK_RIGHT, 0), moveToRightBranch())

      bind(k(VK_TAB, 0), moveToNextHole())
      bind(k(VK_TAB, SHIFT_DOWN_MASK), moveToPreviousHole())

      i(k('r'), reduceCurrentNode())
      i(k('N'), normalizeInPlace())
      i(k('n'), normalizeCopy())
      i(k('D'), duplicateFully())

      i(k('+'), doOperator(s.BasicOperator(logic.Plus)))
      i(k('-'), doOperator(s.BasicOperator(logic.Minus)))
      i(k('*'), doOperator(s.BasicOperator(logic.Times)))

      i(k('S'), doOperator(s.BasicOperator(logic.S(1))))
      i(k('K'), doOperator(s.BasicOperator(logic.K_)))
      i(k('I'), doOperator(s.BasicOperator(logic.I)))
      i(k('Y'), doOperator(s.BasicOperator(logic.Y)))

      i(k('.'), insertApp())

      i(k('e'), stripMystery())
      i(k('E'), stripAllMysteries())
      i(k('C'), clearToMystery())
      i(k('i'), insertChild())
      i(k(VK_UP, ALT_DOWN_MASK), absorbParent())

      i(k('X'), unroot())
      i(k('l'), label())
      i(k('/'), comment())
      i(k(VK_SLASH, CTRL_DOWN_MASK), uncomment())

      i(k('f'), reformatSubtree())

      i(k('b'), showBuryMenu())
      i(k('c'), showRecollectMenu())
      i(k('v'), beginTypingVariable())

      '0' to '9' foreach { d =>
        i(k(d), beginTypingNumber(Some(d)))
      }

      i(k('['), insertList())
      i(k(':'), insertElement())
      i(k('d'), deleteElement())

      i(k('m'), doOperator(s.BasicOperator(logic.Eliminator)))

      // TODO: Something a tad more general.
      i(k(VK_F1, 0), doOperator(s.BasicOperator(logic.MaybeNothing)))
      i(k(VK_F2, 0), doOperator(s.BasicOperator(logic.MaybeJust)))

      i(k(VK_Z, CTRL_DOWN_MASK), undo())
      i(k(VK_Y, CTRL_DOWN_MASK), redo())
    }

    {
      var pressingGroup = Option[DraggedGroup](null)

      val mouse = new MouseListener with MouseMotionListener {
        def mouseExited(p1: MouseEvent) {}
        def mouseClicked(p1: MouseEvent) {}
        def mouseEntered(p1: MouseEvent) {}
        def mousePressed(ev: MouseEvent) {
          requestFocus()

          if (ev.isControlDown) {
            makeNewRootAtPoint(ev.getX, ev.getY)
          }
          else if (ev.isAltDown) {
            bubbleAt(ev.getX, ev.getY) foreach { target =>
              joinTo(target)
            }
          }
          else {
            val here = bubbleAt(ev.getX, ev.getY)
            jumpFocus(here)
            pressingGroup = here map { bubble =>
              DraggedGroup(computeGroup(bubble), ev.getX, ev.getY)
            }
          }

          repaint()
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
