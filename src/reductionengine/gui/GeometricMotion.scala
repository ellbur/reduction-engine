
package reductionengine.gui

import java.awt.Point

trait GeometricMotion { this: Editor =>
  def moveCurrentGroup(dx: Int, dy: Int): Boolean = {
    currentGroup match {
      case Some(group) =>
        group foreach { bubble =>
          editingState.get(bubble).now foreach { editing =>
            editing.x() = editing.x.now + dx
            editing.y() = editing.y.now + dy
          }
        }
        true
      case None =>
        false
    }
  }

  val moveCurrentGroupUp = Action("Slide subtree up", Soft) {
    if (moveCurrentGroup(0, -10))
      AsDescribed
    else
      NotPerformed
  }

  val moveCurrentGroupDown = Action("Slide subtree down", Soft) {
    if (moveCurrentGroup(0, +10))
      AsDescribed
    else
      NotPerformed
  }

  val moveCurrentGroupLeft = Action("Slide subtree left", Soft) {
    if (moveCurrentGroup(-10, 0))
      AsDescribed
    else
      NotPerformed
  }

  val moveCurrentGroupRight = Action("Slide subtree right", Soft) {
    if (moveCurrentGroup(+10, 0))
      AsDescribed
    else
      NotPerformed
  }

  def moveGroup(group: Traversable[BubbleEditing], dx: Int, dy: Int) {
    group foreach { editing =>
      editing.x() = editing.x.now + dx
      editing.y() = editing.y.now + dy
    }
  }
}
