
package reductionengine.gui

trait GeometricMotion { this: Editor =>
  def moveCurrentGroupUp() {
    currentGroup foreach (_ foreach (_ move (0, -10)))
  }

  def moveCurrentGroupDown() {
    currentGroup foreach (_ foreach (_ move (0, 10)))
  }

  def moveCurrentGroupLeft() {
    currentGroup foreach (_ foreach (_ move (-10, 0)))
  }

  def moveCurrentGroupRight() {
    currentGroup foreach (_ foreach (_ move (10, 0)))
  }

  def moveGroup(group: Traversable[Bubble], dx: Int, dy: Int) {
    group foreach { _.move(dx, dy) }
  }
}
