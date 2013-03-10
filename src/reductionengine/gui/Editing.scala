
package reductionengine.gui

trait Editing { this: Editor =>
  def makeNewRootAtPoint(x: Int, y: Int) {
    val mystery = BubbleContainer(Mystery(x + 50, y + 10))
    val it = BubbleContainer(Root(mystery, x, y))
    // TODO: smart positioning
    bubbles ++= Seq(it, mystery)
    roots += it

    focusedBubble = Some(mystery)
    focusedChild = None
    focusedParent = Some(it)
  }

  def doPlus() {
    focusedBubble match {
      case Some(it@BubbleContainer(m: Mystery)) =>
        replace(it, App ~ (App ~(Plus ~, Mystery ~), Mystery ~))
      case _ =>
    }
  }
}
