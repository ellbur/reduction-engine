
package reductionengine.gui

import java.awt.{Point, Component, Graphics2D}
import reactive.Var

trait Bubbles { this: Editor =>
  object sugar extends reductionengine.sugar.Sugar {
    type NodeType = Bubble
  }
  import sugar.SugarNode
  import sugar.logic

  trait Bubble extends logic.NodeLike with sugar.SugarNodeLike {
    lazy val children: Seq[Bubble] = childEdges map (_.target)
    val childEdges: Seq[Edge]

    def withChildren(children: Seq[Bubble]): Bubble
    def transformChildren(f: Seq[Bubble] => Seq[Bubble]) = withChildren(f(children))
    def hasChild(c: Bubble) = children.exists(_ == c)

    val toSugarNode: SugarNode
    lazy val toNode: logic.Node = toSugarNode.toNode

    def edit(startX: Int, startY: Int): BubbleEditing
  }

  trait BubbleEditing {
    val x: Var[Int]
    val y: Var[Int]
    def freeze: FrozenBubbleEditing
    def render(g: Graphics2D, hsaFocus: Boolean): BubbleRendering
  }

  trait FrozenBubbleEditing {
    val x: Int
    val y: Int
  }

  trait Edge {
    val target: Bubble
  }

  def basicEdge(_target: =>Bubble) = new Edge {
    val target = _target
  }

  trait DefaultEditing extends BubbleEditing {
    val xInit: Int
    val yInit: Int
    val x = Var[Int](xInit)
    val y = Var[Int](yInit)
    def freeze = new DefaultFrozenEditing(x.now, y.now)
  }

  class DefaultFrozenEditing(val x: Int, val y: Int) extends FrozenBubbleEditing
}
