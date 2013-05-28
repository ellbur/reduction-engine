
package reductionengine.gui

import scala.collection.mutable
import java.awt.Point

trait GarbageCollection { self: Editor =>
  import self.{sugar => s}
  import s.RNode

  def garbageCollect(roots: Set[Bubble], bubbles: Set[Bubble]): Set[Bubble] = {
    val desired = mutable.Set[Bubble]()

    def visit(b: Bubble) {
      if (!desired.contains(b)) {
        desired += b
        for (child <- b.children.now)
          visit(child)
      }
    }
    for (root <- roots)
      visit(root)

    desired.toSet
  }
}
