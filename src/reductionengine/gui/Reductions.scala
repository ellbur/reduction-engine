
package reductionengine.gui

import collection.mutable
import reactive.{EventSource, EventStream}

trait Reductions { self: Editor =>
  import sugar.{RNode, FocusedRNode}
  import sugar.SugarNode
  import sugar.logic
  import sugar.{NewNode => NN}
  import logic.ReductionPossibility
  import self.{sugar => s}
  import sugar.{LocalNode => l}

  def manifest(node: sugar.LocalNode, location: Point, children: Seq[Bubble]): Bubble = node match {
    case l.IntLiteral(n) => new IntLiteral(location, n)
    case l.Mystery(n) => new Mystery(location, n)
    case l.ApicalOperator(op) => new ApicalOperator(location, op, children)
    case l.Pure(of) => new Pure(location, of, children)
    case l.NumberEditor(id, s) => new NumberEditor(location, id, s)
    case l.AntiPureNameEditor(idiomKind, progress) => new AntiPureNameEditor(location, idiomKind, progress, children(0))
    case l.PureNameEditor(idiomKind, progress) => new PureNameEditor(location, idiomKind, progress, children(0))
    case l.VariableEditor(id, s) => new VariableEditor(location, id, s)
    case l.Variable(name) => new Variable(location, name)
  }

  val doReplace = new EventSource[(Bubble, FocusedRNode)]

  // This is a huge, steaming mess.
  // Oh well.
  doReplace foreach { case (site, focusedReplacement) =>
    import editingState._

    val replacement = focusedReplacement.rnode
    val siteLocation = site.location.now

    val remapping = mutable.Map[RNode, Bubble]()
    val disturbed = mutable.Set[Bubble]()
    def remap(rn: RNode): Bubble = remapping.getOrElseUpdate(rn, rn match {
      case s.AlreadyThere(bubble) =>
        disturbed += bubble
        bubble
      case s.NewNode(n, _) =>
        val children = n.children map (remap(_))
        children foreach (disturbed += _)
        val bubble = manifest(n.local, siteLocation, children)
        disturbed += bubble
        bubble
    })
    val replacer = remap(replacement)

    val toBegin = visibleBubbles.now

    def replaced(b: Bubble) =
      if (b == site) replacer
      else b

    toBegin foreach { bubble =>
      bubble.children() = bubble.children.now map replaced _
    }
    roots() = roots.now map replaced _

    val gced = garbageCollect(roots.now, toBegin)

    reposition(gced, disturbed.toSet, None)

    visibleBubbles() = gced
    jumpFocus(focusedReplacement.focus map (remap(_)))
  }

  def findReductionPossibilities(bc: Bubble):
    Seq[ReductionPossibility] =
  {
    logic.StandardReductions.find(logic.AlreadyThere(bc): logic.RNode).now.toSeq
  }
}
