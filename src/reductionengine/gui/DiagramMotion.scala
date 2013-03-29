
package reductionengine.gui

trait DiagramMotion { this: Editor =>
  def identifyAParent(of: Bubble): Option[Bubble] = {
    for (b <- bubbles sortBy (_.x)) {
      if (b.hasChild(of))
        return Some(b);
    }
    return None
  }

  def identifyAChild(of: Bubble): Option[Bubble] = {
    (of.children.toSeq sortBy (_.x)).toList match {
      case Nil => None
      case fst :: rest => Some(fst)
    }
  }

  def moveToParent() {
    focusedParent.now foreach { parent =>
      focusedBubble.now foreach { bubble =>
        focusedChild() = Some(bubble)
      }
      focusedBubble() = Some(parent)
      focusedParent() = identifyAParent(parent)

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
    focusedChild.now foreach { child =>
      focusedBubble.now foreach { bubble =>
        focusedParent() = Some(bubble)
      }
      focusedBubble() = Some(child)
      focusedChild() = identifyAChild(child)

      updateBubblyThings()
    }
  }

  def moveToBranch(by: Int) {
    // Move the parent pointer
    focusedBubble.now foreach { bubble =>
      focusedParent.now match {
        case None =>
          focusedParent() = identifyAParent(bubble)
        case Some(parent) =>
          val ps = bubble.parents.toIndexedSeq.sortBy(_.x)
          val here = ps.indexOf(parent)
          if (here != -1) {
            focusedParent() = Some(ps((here + by + ps.length) % ps.length))
          }
      }
    }

    // Move the child pointer
    focusedBubble.now foreach { bubble =>
      focusedChild.now match {
        case None =>
          focusedChild() = identifyAChild(bubble)
        case Some(child) =>
          val chs = bubble.children.toIndexedSeq.sortBy(_.x)
          val here = chs.indexOf(child)
          if (here != -1) {
            focusedChild() = Some(chs((here + by + chs.length) % chs.length))
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

  def moveToNextHole() {
    // TODO
  }

  def moveToPreviousHole() {
    // TODO
  }
}
