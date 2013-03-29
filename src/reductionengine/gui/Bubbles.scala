
package reductionengine.gui

import java.awt.{Component, Graphics2D}

trait Bubbles { this: Editor =>
  object sugar extends reductionengine.sugar.Sugar {
    type NodeType = Bubble
    val nodeLike = bubbleNodeLike
  }
  import sugar.SugarNode
  import sugar.logic

  trait Bubble extends {
    def children: Traversable[Bubble] = childEdges map (_.target)
    def childEdges: Traversable[Edge]

    var x: Int
    var y: Int

    def hasChild(b: Bubble): Boolean =
      ! children.find(_ == b).isEmpty

    def haveFocus = focusedBubble.now map (_ == this) getOrElse false

    def hasFocus = haveFocus

    def isFocusedParent: Boolean =
      focusedParent.now map (_ == this) getOrElse false

    def isFocusedChild =
      focusedChild.now map (_ == this) getOrElse false

    def parents: Traversable[Bubble] = bubbles filter (_.hasChild(this))
    def parentEdges: Traversable[Edge] =
      bubbles flatMap (_.childEdges) filter (_.target == this)

    def render(g: Graphics2D): BubbleRendering

    def toSugarNode: SugarNode
    def toNode: logic.Node = toSugarNode.toNode

    val components: Seq[Component] = Seq()
    def move(dx: Int, dy: Int) {
      x += dx
      y += dy
    }

    def receiveFocus() { }
  }

  object bubbleNodeLike extends logic.NodeLike {
    def toNode(rb: Bubble) = rb.toNode
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
