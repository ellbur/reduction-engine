
package reductionengine.gui

import java.awt.{Component, Rectangle, Graphics2D}
import reductionengine.logic
import reductionengine.sugar
import reductionengine.sugar.SugarNode

trait Bubbles { this: Editor =>
  trait Bubble extends {
    def children: Traversable[Bubble] = childEdges map (_.target)
    def childEdges: Traversable[Edge]

    var x: Int
    var y: Int

    def hasChild(b: Bubble): Boolean =
      ! children.find(_ == b).isEmpty

    def haveFocus = focusedBubble map (_ == this) getOrElse false

    def hasFocus = haveFocus

    def isFocusedParent: Boolean =
      focusedParent map (_ == this) getOrElse false

    def isFocusedChild =
      focusedChild map (_ == this) getOrElse false

    def parents: Traversable[Bubble] = bubbles filter (_.hasChild(this))
    def parentEdges: Traversable[Edge] =
      bubbles flatMap (_.childEdges) filter (_.target == this)

    def render(g: Graphics2D): BubbleRendering

    def toSugarNode: SugarNode[Bubble]
    def toNode: logic.Node[logic.Replacement[Bubble]] = toSugarNode.toNode

    val components: Seq[Component] = Seq()
    def move(dx: Int, dy: Int) {
      x += dx
      y += dy
    }

    def receiveFocus() { }
  }

  implicit object bubbleNodeLike extends logic.NodeLike[Bubble] {
    def toNode(rb: logic.Replacement[Bubble]) = rb match {
      case logic.NewNode(n) => n
      case logic.AlreadyThere(b) => b.toNode
    }
  }

  trait Edge {
    def target: Bubble
    def substitute(next: Bubble): Unit
    def deleteIfReasonable() { }
  }

  def basicEdge(_target: =>Bubble, _substitute: Bubble=>Unit) = new Edge {
    def target = _target
    def substitute(next: Bubble) = _substitute(next)
  }
}
