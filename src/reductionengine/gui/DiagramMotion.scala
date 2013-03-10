
package reductionengine.gui

trait DiagramMotion { this: Editor =>
  def identifyAParent(of: BubbleContainer): Option[BubbleContainer] = {
    for (b <- bubbles sortBy (_.bubble.x)) {
      if (b.bubble.hasChild(of))
        return Some(b);
    }
    return None
  }

  def identifyAChild(of: Bubble): Option[BubbleContainer] = {
    (of.children.toSeq sortBy (_.bubble.x)).toList match {
      case Nil => None
      case fst :: rest => Some(fst)
    }
  }

  def moveToParent() {
    focusedParent foreach { parent =>
      focusedBubble foreach { bubble =>
        focusedChild = Some(bubble)
      }
      focusedBubble = Some(parent)
      focusedParent = identifyAParent(parent)

      updateBubblyThings()
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

  def moveToChild() {
    focusedChild foreach { child =>
      focusedBubble foreach { bubble =>
        focusedParent = Some(bubble)
      }
      focusedBubble = Some(child)
      focusedChild = identifyAChild(child.bubble)

      updateBubblyThings()
    }
  }

  def moveToBranch(by: Int) {
    // Move the parent pointer
    focusedBubble foreach { bubble =>
      focusedParent match {
        case None =>
          focusedParent = identifyAParent(bubble)
        case Some(parent) =>
          val ps = bubble.parents.toIndexedSeq.sortBy(_.bubble.x)
          val here = ps.indexOf(parent)
          if (here != -1) {
            focusedParent = Some(ps((here + by + ps.length) % ps.length))
          }
      }
    }

    // Move the child pointer
    focusedBubble foreach { bubble =>
      focusedChild match {
        case None =>
          focusedChild = identifyAChild(bubble.bubble)
        case Some(child) =>
          val chs = bubble.bubble.children.toIndexedSeq.sortBy(_.bubble.x)
          val here = chs.indexOf(child)
          if (here != -1) {
            focusedChild = Some(chs((here + by + chs.length) % chs.length))
          }
      }
    }
  }

  def moveToLeftBranch() {
    moveToBranch(-1)
  }

  def moveToRightBranch() {
    moveToBranch(+1)
  }
}
