
package reductionengine.gui

import scala.collection.mutable

trait GarbageCollection { this: Editor =>
  def runGC() {
    val interestingNodes = mutable.HashSet[BubbleContainer]()

    def see(b: BubbleContainer) {
      if (!(interestingNodes contains b)) {
        interestingNodes += b
        for (c <- b.bubble.children)
          see(c)
      }
    }

    roots foreach see _

    val lost = bubbles.toSet -- interestingNodes.toSet

    val movedFocus = focusedBubble map (stepOutOfDeletion(lost, _)) getOrElse false
    focusedBubble foreach { bubble =>
      if (movedFocus) {
        focusedParent = identifyAParent(bubble)
        focusedChild = identifyAChild(bubble.bubble)
      }
      else {
        focusedParent foreach { parent =>
          if (lost contains parent)
            focusedParent = identifyAParent(bubble)
        }
        focusedChild foreach { child =>
          if (lost contains child)
            focusedChild = identifyAChild(bubble.bubble)
        }
      }
    }

    bubbles.clear()
    bubbles ++= interestingNodes.toSeq

    updateBubblyThings()
  }

  def stepOutOfDeletion(lost: Set[BubbleContainer], b: BubbleContainer): Boolean = {
    var here: BubbleContainer = b
    var moved: Boolean = false
    while (lost contains here) {
      here = focusedParent match {
        case Some(p) if p.bubble.hasChild(here) => p
        case _ => identifyAParent(here) getOrElse roots(0)
      }
      focusedBubble = Some(here)
      moved = true
    }
    moved
  }
}
