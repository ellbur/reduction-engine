
package reductionengine.gui

import scala.collection.mutable

trait FocusAndGroups { this: Editor =>
  def computeGroup(b: Bubble): Traversable[Bubble] = {
    val group = mutable.HashSet[Bubble]()
    def takeCareOf(child: Bubble) {
      if (!(group contains child)) {
        group += child
        for (next <- child.children)
          takeCareOf(next.bubble)
      }
    }
    takeCareOf(b)
    group
  }

  def currentGroup: Option[Traversable[Bubble]] =
    focusedBubble map (b => computeGroup(b.bubble))
}
