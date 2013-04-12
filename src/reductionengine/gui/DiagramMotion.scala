
package reductionengine.gui

import scala.Some

trait DiagramMotion { this: Editor =>
  def parentsOf(bubble: Bubble) =
    editingState.visibleBubbles.toMap.now map (_._1) filter (_.hasChild(bubble))

  def identifyAParent(of: Bubble): Option[Bubble] = {
    import editingState.visibleBubbles

    val choices = visibleBubbles.toMap.now filter (_._1.hasChild(of))
    val inOrder = choices.toList sortBy (_._2.x.now)

    inOrder match {
      case b :: bs => Some(b._1)
      case _ => None
    }
  }

  def identifyAChild(of: Bubble): Option[Bubble] = {
    val children = of.children.toSeq
    children.toList match {
      case Nil => None
      case fst :: rest => Some(fst)
    }
  }

  val moveToParent = Action("Move to parent", Nada) {
    import editingState._

    focusedParent.now match {
      case Some(parent) =>
        getEdited(parent).now match {
          case Some(edited) =>
            focusedBubble.now foreach { case EditedBubble(bubble, _) =>
              focusedChild() = Some(bubble)
            }
            focusedBubble() = Some(edited)
            focusedParent() = identifyAParent(parent)
            AsDescribed
          case None =>
            NotPerformed
        }
      case None =>
        NotPerformed
    }
  }

  def moveToLeftChild() {
    // TODO
  }

  def moveToRightChild() {
    // TODO
  }

  def moveToMiddleChild() {
    // TODO
  }

  val moveToChild = Action("Move to child", Nada) {
    import editingState._

    focusedChild.now match {
      case Some(child) =>
        getEdited(child).now match {
          case Some(edited) =>
            focusedBubble.now foreach { case EditedBubble(bubble, _) =>
              focusedParent() = Some(bubble)
            }
            focusedBubble() = Some(edited)
            focusedChild() = identifyAChild(child)
            AsDescribed
          case None =>
            NotPerformed
        }
      case None =>
        NotPerformed
    }
  }

  def moveToBranch(by: Int) {
    import editingState._

    focusedBubble.now foreach { case EditedBubble(bubble, _) =>
      // Move the parent pointer
      focusedParent.now match {
        case None =>
          focusedParent() = identifyAParent(bubble)
        case Some(parent) =>
          val ps = parentsOf(bubble).toIndexedSeq
          val positioned = ps map (getEdited(_).now) collect {
            case Some(e) => e
          } sortBy (_.editing.x.now) map (_.bubble)

          val here = ps.indexOf(parent)
          if (here != -1) {
            focusedParent() = Some(ps((here + by + ps.length) % ps.length))
          }
      }

      // Move the child pointer
      focusedChild.now match {
        case None =>
          focusedChild() = identifyAChild(bubble)
        case Some(child) =>
          val chs = bubble.children.toIndexedSeq
          val here = chs.indexOf(child)
          if (here != -1) {
            focusedChild() = Some(chs((here + by + chs.length) % chs.length))
          }
      }
    }
  }

  val moveToLeftBranch = Action("Cycle left", Nada) {
    moveToBranch(-1)
    AsDescribed
  }

  val moveToRightBranch = Action("Cycle right", Nada) {
    moveToBranch(+1)
    AsDescribed
  }

  def moveToNextHole() {
    // TODO
  }

  def moveToPreviousHole() {
    // TODO
  }

  def jumpFocus(at: Option[Bubble]) {
    import editingState._

    at match {
      case None =>
        focusedBubble() = None
        focusedChild() = None
        focusedParent() = None
      case Some(bubble) =>
        editingState.getEdited(bubble).now match {
          case Some(edited) =>
            focusedBubble() = Some(edited)
            focusedParent() = identifyAParent(bubble)
            focusedChild() = identifyAChild(bubble)
          case None =>
            focusedBubble() = None
            focusedChild() = None
            focusedParent() = None
        }
    }
  }
}
