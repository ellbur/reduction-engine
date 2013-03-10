
package reductionengine.gui

import collection.mutable.ArrayBuffer
import javax.swing.SwingWorker

trait OurBubbles { this: Editor =>
  val plus = BubbleContainer(Plus(100, 300))
  val a = BubbleContainer(IntLiteral(3, 300, 300))
  val plusA = BubbleContainer(App(plus, a, 200, 200))
  val b = BubbleContainer(IntLiteral(5, 400, 200))
  val plusAB = BubbleContainer(App(plusA, b, 300, 100))

  val roots = ArrayBuffer[BubbleContainer](BubbleContainer(Root(plusAB, 250, 90)))

  val bubbles = ArrayBuffer[BubbleContainer](
    roots(0),
    plus,
    a,
    plusA,
    b,
    plusAB
  )

  var focusedBubble = Option[BubbleContainer](plusA)
  var focusedChild = Option[BubbleContainer](plus)
  var focusedParent = Option[BubbleContainer](plusAB)

  def updateBubblyThings() {
    updateFocusedReductions()
  }

  var focusedReductions: Traversable[ReductionPossibility] = Seq()
  def updateFocusedReductions() {
    val worker = new SwingWorker[Traversable[ReductionPossibility],Any] {
      def doInBackground =
        focusedBubble.toTraversable flatMap (findReductionPossibilities(_))

      override def done() {
        focusedReductions = get()
        repaint()
      }
    }
    worker.execute()
  }
}
