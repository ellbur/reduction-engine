
package reductionengine.gui

import java.awt.Point

trait GeometricMotion { this: Editor =>
  def moveCurrentGroup(dx: Int, dy: Int): Boolean = {
    currentGroup match {
      case Some(group) =>
        group foreach { bubble =>
          bubble.location() = bubble.location.now + (dx, dy)
        }
        true
      case None =>
        false
    }
  }

  def moveCurrentGroupUp() {
    if (moveCurrentGroup(0, -10))
      actSoft("Slide subtree up")
  }

  def moveCurrentGroupDown() {
    if (moveCurrentGroup(0, +10))
      actSoft("Slide subtree down")
  }

  def moveCurrentGroupLeft() {
    if (moveCurrentGroup(-10, 0))
      actSoft("Slide subtree left")
  }

  def moveCurrentGroupRight() {
    if (moveCurrentGroup(+10, 0))
      actSoft("Slide subtree right")
  }

  def moveGroup(group: Traversable[Bubble], dx: Int, dy: Int) {
    group foreach { bubble =>
      bubble.location() = bubble.location.now + (dx, dy)
    }
  }
}
