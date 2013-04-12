
package reductionengine.gui

import scala.collection.mutable

trait FocusAndGroups { this: Editor =>
  def computeGroup(b: Bubble): Traversable[Bubble] = {
    val group = mutable.HashSet[Bubble]()
    def takeCareOf(child: Bubble) {
      if (!(group contains child)) {
        group += child
        for (next <- child.children)
          takeCareOf(next)
      }
    }
    takeCareOf(b)
    group
  }

  import editingState.focusedBubble

  def currentGroup: Option[Traversable[Bubble]] =
    focusedBubble.now map (eb => computeGroup(eb.bubble))
}
