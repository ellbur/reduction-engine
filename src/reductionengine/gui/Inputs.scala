
package reductionengine.gui

import javax.swing._
import java.awt.event._

trait Inputs { self: Editor =>
  import sugar.logic
  import self.{sugar => s}

  sealed trait Hardness
  case object Hard extends Hardness
  case object Soft extends Hardness
  case object Nada extends Hardness

  sealed trait ResultingDescription
  case object AsDescribed extends ResultingDescription
  case object NotPerformed extends ResultingDescription
  case class DescribedAs(description: String) extends ResultingDescription

  case class Action(description: String, hardness: Hardness)(performer: => ResultingDescription) {
    def perform() {
      def report(as: String) {
        hardness match {
          case Hard => actHard(as)
          case Soft => actSoft(as)
          case Nada =>
        }
      }

      performer match {
        case NotPerformed =>
        case AsDescribed =>
          report(description)
        case DescribedAs(newDescription) =>
          report(newDescription)
      }
    }
  }

  case class KeyInput(
   keyDescription: String,
   strokes: Traversable[KeyStroke],
   description: String,
   action: KeyStroke => Unit)
  {
    def perform(stroke: KeyStroke) {
      action(stroke)
    }
  }

  lazy val keyInputs = {
    import KeyEvent._
    import InputEvent._

    object ways {
      def k(c: Char) = KeyStroke.getKeyStroke(c)
      def k(c: Int, mods: Int) = KeyStroke.getKeyStroke(c, mods)

      def i(s: KeyStroke, action: Action)
        = KeyInput(s.toString, s::Nil, action.description, _ => action.perform())
    }
    import ways._

    Seq(
      i(k(VK_UP, SHIFT_DOWN_MASK), moveCurrentGroupUp),
      i(k(VK_DOWN, SHIFT_DOWN_MASK), moveCurrentGroupDown),
      i(k(VK_LEFT, SHIFT_DOWN_MASK), moveCurrentGroupLeft),
      i(k(VK_RIGHT, SHIFT_DOWN_MASK), moveCurrentGroupRight),

      i(k(VK_UP, 0), moveToParent),
      i(k(VK_DOWN, 0), moveToChild),
      i(k(VK_LEFT, 0), moveToLeftBranch),
      i(k(VK_RIGHT, 0), moveToRightBranch),

      // TODO
      //bind(VK_TAB, 0, moveToNextHole())
      //bind(VK_TAB, SHIFT_DOWN_MASK, moveToPreviousHole())

      i(k('r'), reduceCurrentNode),
      i(k('N'), normalizeInPlace),
      i(k('n'), normalizeCopy),
      i(k('D'), duplicateFully),

      i(k('+'), doOperator(s.BasicOperator(logic.Plus))),
      i(k('-'), doOperator(s.BasicOperator(logic.Minus))),
      i(k('*'), doOperator(s.BasicOperator(logic.Times))),

      i(k('S'), doOperator(s.BasicOperator(logic.S(1)))),
      i(k('K'), doOperator(s.BasicOperator(logic.K(1, Seq(false))))),
      i(k('I'), doOperator(s.BasicOperator(logic.I))),
      i(k('Y'), doOperator(s.BasicOperator(logic.Y))),

      i(k('.'), insertApp),

      i(k('e'), stripMystery),
      i(k('C'), clearToMystery),
      i(k('i'), insertChild),

      i(k('f'), reformatSubtree),

      i(k('b'), showBuryMenu),
      i(k('c'), showRecollectMenu),
      i(k('v'), beginTypingVariable),

      {
        val keys = '0' to '9' map (k(_))
        def digitOf(ks: KeyStroke) = {
          val char = ks.getKeyChar
          if (char.isDigit)
            Some(char)
          else
            None
        }
        KeyInput("0..9,#", keys, "Insert number", { stroke =>
          beginTypingNumber(digitOf(stroke))
        })
      }
    )
  }

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

    keyInputs foreach { input =>
      input.strokes foreach { stroke =>
        bind(stroke, input.perform(stroke))
      }
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
              DraggedGroup(computeGroup(bubble) flatMap (editingState.get(_).now), ev.getX, ev.getY)
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

  case class DraggedGroup(members: Traversable[BubbleEditing], x: Int, y: Int)
}
