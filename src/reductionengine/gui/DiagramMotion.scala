
package reductionengine.gui

import scala.Some

trait DiagramMotion { this: Editor =>
  def parentsOf(bubble: Bubble) =
    editingState.visibleBubbles.now filter (_.hasChild(bubble))

  def identifyAParent(of: Bubble): Option[Bubble] = {
    import editingState.visibleBubbles

    val choices = visibleBubbles.now filter (_.hasChild(of))
    val inOrder = choices.toList sortBy (_.location.now.x)

    inOrder match {
      case b :: bs => Some(b)
      case _ => None
    }
  }

  def identifyAChild(of: Bubble): Option[Bubble] = {
    val children = of.children.now.toSeq
    children.toList match {
      case Nil => None
      case fst :: rest => Some(fst)
    }
  }

  def moveToParent() {
    import editingState._
    focusedParent.now match {
      case Some(parent) =>
        focusedBubble.now foreach { bubble =>
          focusedChild() = Some(bubble)
        }
        focusedBubble() = Some(parent)
        focusedParent() = identifyAParent(parent)
      case None =>
    }
  }

  def moveToChild() {
    import editingState._

    focusedChild.now match {
      case Some(child) =>
        focusedBubble.now foreach { bubble =>
          focusedParent() = Some(bubble)
        }
        focusedBubble() = Some(child)
        focusedChild() = identifyAChild(child)
      case None =>
    }
  }

  def moveToBranch(by: Int) {
    import editingState._

    focusedBubble.now foreach { bubble =>
      // Move the parent pointer
      focusedParent.now match {
        case None =>
          focusedParent() = identifyAParent(bubble)
        case Some(parent) =>
          val ps = parentsOf(bubble).toIndexedSeq
          val positioned = ps sortBy (_.location.now.x)

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
          val chs = bubble.children.now.toIndexedSeq
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

  sealed trait Direction
  case object Forward extends Direction
  case object Backward extends Direction

  def moveToHole(direction: Direction) {
    import editingState._
    focusedBubble.now match {
      case None => message("Nothing selected")
      case Some(here) =>
        def searchAtBubble(bubble: Bubble, useFocusedParent: Boolean): Boolean = {
          val parents = (visibleBubbles.now filter (_.hasChild(bubble))).toSeq
          val sortedParents =
            if (useFocusedParent)
              parents.sortBy(p => if (p.isFocusedParent) 0 else 1)
            else
              parents

          def searchParents(parents: List[Bubble]): Boolean = parents match {
            case Nil => false
            case parent :: parents =>
              val siblings = parent.children.now
              val ourIndex = siblings.indexOf(bubble)
              if (ourIndex == -1)
                searchParents(parents)
              else {
                val olderSiblings = direction match {
                  case Forward => siblings.drop(ourIndex + 1)
                  case Backward => siblings.take(ourIndex)
                }
                def searchSiblings(siblings: List[Bubble]): Boolean = siblings match {
                  case Nil => false
                  case sibling :: siblings =>
                    sibling match {
                      case hole: Mystery =>
                        jumpFocus(Some(hole))
                        true
                      case _ =>
                        if (searchSiblings(sibling.children.now.toList))
                          true
                        else
                          searchSiblings(siblings)
                    }
                }
                if (searchSiblings(olderSiblings.toList))
                  true
                else if (searchParents(parents))
                  true
                else
                  searchAtBubble(parent, useFocusedParent = false)
              }
          }
          searchParents(sortedParents.toList)
        }

        searchAtBubble(here, useFocusedParent = true)
    }
  }

  def moveToNextHole() {
    moveToHole(Forward)
  }

  def moveToPreviousHole() {
    moveToHole(Backward)
  }

  def jumpFocus(at: Option[Bubble]) {
    import editingState._

    at match {
      case None =>
        focusedBubble() = None
        focusedChild() = None
        focusedParent() = None
      case Some(bubble) =>
        focusedBubble() = Some(bubble)
        focusedParent() = identifyAParent(bubble)
        focusedChild() = identifyAChild(bubble)
    }
  }
}
