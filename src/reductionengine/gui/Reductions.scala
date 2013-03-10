
package reductionengine.gui

import scala.collection.mutable
import scala.App

trait Reductions { this: Editor =>
  object AppRed {
    def unapply(b: Bubble): Option[(Bubble, Bubble)] = b match {
      case a: App => Some((a.car.bubble, a.cdr.bubble))
      case _ => None
    }
  }

  object PlusRed {
    def unapply(b: Bubble): Boolean = b match {
      case _: Plus => true
      case _ => false
    }
  }

  object IntLiteralRed {
    def unapply(b: Bubble): Option[Int] = b match {
      case i: IntLiteral => Some(i.n)
      case _ => None
    }
  }

  def findReductionPossibilities(at: BubbleContainer): Traversable[ReductionPossibility] = {
    at.bubble match {
      case AppRed(AppRed(PlusRed(), IntLiteralRed(a)), IntLiteralRed(b)) =>
        List(new ReductionPossibility {
          val description: String = "Perform addition"
          def perform() {
            replace(at, IntLiteral(a+b, _, _))
          }
        })
      case _ => List()
    }
  }

  trait ReductionPossibility {
    val description: String
    def perform(): Unit

    override def toString = description.toString
  }

  type UnplacedBubble = (Int, Int) => Bubble

  // TODO: expand with the possibility of putting in a whole tree.
  def replace(at: BubbleContainer, becomes: UnplacedBubble) {
    at.bubble = becomes(at.bubble.x, at.bubble.y)
    runGC()
  }

  sealed trait Replacement
  case class ExistingBubble(is: BubbleContainer) extends Replacement
  case class NewBubble(willBe: (Seq[BubbleContainer], Int, Int) => Bubble, children: Seq[Replacement]) extends Replacement

  def replace(at: BubbleContainer, becomes: Replacement) {
    val newBubbles = mutable.Set[Bubble]()
    val mapping = mutable.Map[Replacement,Bubble]()

    def handle(r: Replacement): Bubble = {
      mapping.get(r) match {
        case Some(already) => already
        case None =>
          val bc = r match {
            case ExistingBubble(it) => it.bubble
            case NewBubble(willBe, children) =>
              willBe(children map (c => BubbleContainer(handle(c))), at.bubble.x, at.bubble.y)
          }

          newBubbles += bc
          mapping(r) = bc

          bc
      }
    }

    at.bubble = handle(becomes)

    runGC()

    updateBubblyThings()
  }

  def reduceCurrentNode() {
    focusedBubble match {
      case Some(bubble) =>
        val available = findReductionPossibilities(bubble)
        available.headOption match {
          case Some(first) =>
            first.perform()
            updateBubblyThings()
          case None =>
            message("No reductions")
        }
      case None =>
        message("Nothing selected")
    }
  }
}
