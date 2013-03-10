
package reductionengine.gui

trait GeometricMotion { this: Editor =>
  def moveCurrentGroupUp() {
    currentGroup foreach (_ foreach (_.y -= 10))
  }

  def moveCurrentGroupDown() {
    currentGroup foreach (_ foreach (_.y += 10))
  }

  def moveCurrentGroupLeft() {
    currentGroup foreach (_ foreach (_.x -= 10))
  }

  def moveCurrentGroupRight() {
    currentGroup foreach (_ foreach (_.x += 10))
  }
}
