
package reductionengine.gui

import java.awt.{Rectangle, Component, Graphics2D}
import reactive.{EventStream, Signal, Var}
import scalaz._
import Scalaz._
import signalutils._
import javax.swing.JComponent
import redosignals._
import RedoSignals._

trait Bubbles { this: Editor =>
  object sugar extends reductionengine.sugar.Sugar {
    type NodeType = Bubble
    type M[+X] = Target[X]
    def now[A](x: M[A]) = x.now
    val monad = implicitly[Monad[Target]]
  }
  import sugar.SugarNode
  import sugar.logic

  var bubbleIDCounter: Int = 0
  def nextBubbleID = {
    val it = bubbleIDCounter
    bubbleIDCounter += 1
    it
  }

  trait Bubble extends sugar.SugarNodeLike {
    val id: Int = nextBubbleID

    val initialChildren: Seq[Bubble]
    lazy val children = new Source[Seq[Bubble]](initialChildren)

    val localNode: Target[sugar.LocalNode]
    lazy val toSugarNode = (localNode |@| children) { (node, children) =>
      node.manifest(children map (sugar.AlreadyThere(_)))
    }

    def hasChild(c: Bubble) = children.now.exists(_ == c)

    val initialLocation: Point
    val location = new Source[Point](initialLocation)
    val locationTarget: Target[Point] = location

    /** Override this to specify components. */
    val components: Traversable[JComponent] = Seq()

    lazy val freeze: Target[FrozenBubble] = (locationTarget |@| localNode |@| children) { (loc, node, children) =>
      FrozenBubble(id, loc, node, children map (_.id))
    }

    def render(g: Graphics2D): BubbleRendering

    def hasFocus = editingState.focusedBubble.now map (_ == this) getOrElse false
    def isFocusedParent = editingState.focusedParent.now map (_ == this) getOrElse false
  }

  case class FrozenBubble(id: Int, loc: Point, node: sugar.LocalNode, children: Seq[Int])

  case class Point(x: Int, y:  Int) {
    def +(dx: Int, dy: Int) = copy(x = x+dx, y = y+dy)
    def +(d: (Int, Int)) = copy(x = x+d._1, y = y+d._2)
  }
  object Point {
    def apply(p: (Int, Int)) = new Point(p._1, p._2)
  }

  case class BubbleRendering(bounds: Rectangle, parentAttachment: Point, childAttachments: Seq[Point])
}
