
package reductionengine.gui

import scala.collection.mutable

trait GarbageCollection { this: Editor =>
  def runGC() {
    val interestingNodes = mutable.HashSet[Bubble]()

    def see(b: Bubble) {
      if (!(interestingNodes contains b)) {
        interestingNodes += b
        for (c <- b.children)
          see(c)
      }
    }

    roots foreach see _

    val lost = bubbles.toSet -- interestingNodes.toSet
    for (b <- lost)
      for (c <- b.components)
        canvas.remove(c)

    bubbles.clear()
    bubbles ++= interestingNodes.toSeq

    updateBubblyThings()
  }
}
