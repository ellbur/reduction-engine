
package reductionengine.gui

import java.awt.Graphics2D

trait Bubbles { this: Editor =>
  trait Bubble {
    def children: Traversable[BubbleContainer]

    var x: Int
    var y: Int

    def hasChild(b: BubbleContainer): Boolean =
      ! children.find(_ == b).isEmpty

    def haveFocus = focusedBubble map (_.bubble == this) getOrElse false

    def hasFocus = haveFocus

    def isFocusedParent: Boolean =
      focusedParent map (_.bubble == this) getOrElse false

    def isFocusedChild =
      focusedChild map (_.bubble == this) getOrElse false

    def render(g: Graphics2D)
    def renderEdges(g: Graphics2D)
  }

  case class BubbleContainer(var bubble: Bubble) {
    def parents: Traversable[BubbleContainer] =
      bubbles filter(_.bubble hasChild this)
  }
}
