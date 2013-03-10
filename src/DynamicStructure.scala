
import collection.mutable
import collection.mutable.ArrayBuffer
import java.awt.event._
import java.awt.{BasicStroke, Graphics, Color, Graphics2D}
import javax.swing._
import net.miginfocom.layout.{CC, LC}
import net.miginfocom.swing.MigLayout
import scala.Some

object DynamicStructure extends scala.App {
  class Editor extends JPanel
    with Bubbles
    with KindsOfBubbles
    with OurBubbles
    with FocusAndGroups
    with Reductions
    with GarbageCollection
    with DiagramMotion
    with Editing
    with GeometricMotion
    with Layout
    with Rendering
    with Keys
    with SomeExperiments

  trait Bubbles { this: Editor =>
    trait Bubble {
      def children: Traversable[BubbleContainer]

      var x: Int
      var y: Int

      def hasChild(b: BubbleContainer): Boolean =
        ! children.find(_ == b).isEmpty

      def haveFocus = focusedBubble map (_.bubble == this) getOrElse false

      def hasFocus = haveFocus

      def isFocusedParent: Boolean =
        focusedParent map (_.bubble == this) getOrElse false

      def isFocusedChild =
        focusedChild map (_.bubble == this) getOrElse false

      def render(g: Graphics2D)
      def renderEdges(g: Graphics2D)
    }

    case class BubbleContainer(var bubble: Bubble) {
      def parents: Traversable[BubbleContainer] =
          bubbles filter(_.bubble hasChild this)
    }
  }

  trait KindsOfBubbles { this: Editor =>
    case class App(
                    var car: BubbleContainer,
                    var cdr: BubbleContainer,
                    var x: Int,
                    var y: Int) extends Bubble with AppRender
    {
      def children = Seq(car, cdr)
    }

    case class IntLiteral(
                           var n: Int,
                           var x: Int,
                           var y: Int) extends Bubble with IntLiteralRender
    {
      def children = Seq()
    }

    case class Plus(var x: Int, var y: Int) extends Bubble with PlusRender
    {
      def children = Seq()
    }

    case class Root(var is: BubbleContainer, var x: Int, var y: Int) extends Bubble with RootRender
    {
      def children = Seq(is)
    }

    case class Mystery(var x: Int, var y: Int) extends Bubble with MysteryRender
    {
      def children = Seq()
    }
  }

  trait OurBubbles { this: Editor =>
    val plus = BubbleContainer(Plus(100, 300))
    val a = BubbleContainer(IntLiteral(3, 300, 300))
    val plusA = BubbleContainer(App(plus, a, 200, 200))
    val b = BubbleContainer(IntLiteral(5, 400, 200))
    val plusAB = BubbleContainer(App(plusA, b, 300, 100))

    val root = BubbleContainer(Root(plusAB, 250, 90))

    val bubbles = ArrayBuffer[BubbleContainer](
      root,
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

  trait FocusAndGroups { this: Editor =>
    def computeGroup(b: Bubble): Traversable[Bubble] = {
      val group = mutable.HashSet[Bubble]()
      def takeCareOf(child: Bubble) {
        if (!(group contains child)) {
          group += child
          for (next <- child.children)
            takeCareOf(next.bubble)
        }
      }
      takeCareOf(b)
      group
    }

    def currentGroup: Option[Traversable[Bubble]] =
      focusedBubble map (b => computeGroup(b.bubble))
  }

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

    import GraphLayout._

    sealed trait Replacement
    case class ExistingBubble(is: BubbleContainer) extends Replacement
    case class NewBubble(willBe: (Seq[BubbleContainer], Int, Int) => BubbleContainer, children: Seq[Replacement]) extends Replacement

    def replace(at: BubbleContainer, becomes: Replacement) {
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

  trait GarbageCollection { this: Editor =>
    def runGC() {
      val interestingNodes = mutable.HashSet[BubbleContainer]()

      def see(b: BubbleContainer) {
        if (!(interestingNodes contains b)) {
          interestingNodes += b
          for (c <- b.bubble.children)
            see(c)
        }
      }

      see(root)

      val lost = bubbles.toSet -- interestingNodes.toSet

      val movedFocus = focusedBubble map (stepOutOfDeletion(lost, _)) getOrElse false
      focusedBubble foreach { bubble =>
        if (movedFocus) {
          focusedParent = identifyAParent(bubble)
          focusedChild = identifyAChild(bubble.bubble)
        }
        else {
          focusedParent foreach { parent =>
            if (lost contains parent)
              focusedParent = identifyAParent(bubble)
          }
          focusedChild foreach { child =>
            if (lost contains child)
              focusedChild = identifyAChild(bubble.bubble)
          }
        }
      }

      bubbles.clear()
      bubbles ++= interestingNodes.toSeq

      updateBubblyThings()
    }

    def stepOutOfDeletion(lost: Set[BubbleContainer], b: BubbleContainer): Boolean = {
      var here: BubbleContainer = b
      var moved: Boolean = false
      while (lost contains here) {
        here = focusedParent match {
          case Some(p) if p.bubble.hasChild(here) => p
          case _ => identifyAParent(here) getOrElse root
        }
        focusedBubble = Some(here)
        moved = true
      }
      moved
    }
  }

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

  trait Editing { this: Editor =>
    def makeNewRootAtPoint(x: Int, y: Int) {
      val mystery = BubbleContainer(Mystery(x + 50, y + 10))
      val it = BubbleContainer(Root(mystery, x, y))
      // TODO: smart positioning
      bubbles ++= Seq(it, mystery)

      focusedBubble = Some(mystery)
      focusedChild = None
      focusedParent = Some(it)
    }
  }

  trait GeometricMotion { this: Editor =>
    def moveCurrentGroupUp() {
      currentGroup foreach (_ foreach (_.y -= 10))
    }

    def moveCurrentGroupDown() {
      currentGroup foreach (_ foreach (_.y += 10))
    }

    def moveCurrentGroupLeft() {
      currentGroup foreach (_ foreach (_.x -= 10))
    }

    def moveCurrentGroupRight() {
      currentGroup foreach (_ foreach (_.x += 10))
    }
  }

  trait Layout { this: Editor =>
    class MainCanvas extends JPanel with SetupKeys {
      override def paintComponent(_g: Graphics) {
        val g = _g.asInstanceOf[Graphics2D]

        g.setColor(Color.white)
        g.fillRect(0, 0, getSize().width, getSize().height)

        for (bc <- bubbles)
          bc.bubble.renderEdges(g)

        for (bc <- bubbles)
          bc.bubble.render(g)

        val focused = focusedReductions.toSeq
        if (focused.length > 0) {
          println("Drawing focused reductions!")

          val rows = focused.toIndexedSeq.zipWithIndex map {
            case (reduction, i) =>
              <tr>
                <td>{if (i==0) "r" else ""}</td>
                <td>{(i+1).toString}</td>
                <td>{reduction.description}</td>
              </tr>
          }

          val html = <html><table>{rows}</table></html>

          println(html)

          val label = new JLabel(html.toString)
          label.setSize(label.getPreferredSize)
          val tr = g.getTransform
          val where = focusedBubble match {
            case Some(bubble) => (bubble.bubble.x + 40, bubble.bubble.y - label.getHeight/2)
            case None => (20, label.getHeight + 20)
          }
          g.translate(where._1, where._2)
          label.paint(g)
          g.setTransform(tr)
        }
      }
    }

    val messageLabel = new JLabel("Ready.")

    setLayout(new MigLayout((new LC).insets("0")))
    add(new MainCanvas, (new CC).cell(0, 0).grow().push())
    add(messageLabel, (new CC).cell(0, 1).growX())

    def message(text: String) {
      messageLabel setText text
    }
  }

  trait Rendering { this: Editor =>
    trait AppRender { this: App =>
      def render(g: Graphics2D) {
        if (haveFocus) {
          g.setColor(new Color(0, 100, 0))
          g.fillOval(x-4, y-4, 8, 8)
        }
        else {
          g.setColor(Color.black)
          g.fillOval(x-2, y-2, 4, 4)
        }
      }

      def renderEdges(g: Graphics2D) {
        connectLine(g, this, car.bubble)
        connectLine(g, this, cdr.bubble)
      }
    }

    trait IntLiteralRender { this: IntLiteral =>
      def render(g: Graphics2D) {
        renderTextBubble(g, haveFocus, n.toString, x, y)
      }

      def renderEdges(g: Graphics2D) { }
    }

    trait PlusRender { this: Plus =>
      def render(g: Graphics2D) {
        renderTextBubble(g, haveFocus, "+", x, y)
      }

      def renderEdges(g: Graphics2D) { }
    }

    trait RootRender { this: Root =>
      def render(g: Graphics2D) {
        g.setColor(Color.yellow)
        g.fillRect(x-5, y-5, 5, 5)
        if (haveFocus) {
          g.setColor(Color.red)
          g.setStroke(new BasicStroke(2))
          g.drawRect(x-5, y-5, 5, 5)
        }
      }

      def renderEdges(g: Graphics2D) {
        connectLine(g, this, is.bubble)
      }
    }

    trait MysteryRender { this: Mystery =>
      def render(g: Graphics2D) {
        renderTextBubble(g, haveFocus, "?", x, y, bgColor=new Color(200, 200, 200))
      }

      def renderEdges(g: Graphics2D) { }
    }

    val bubbleBGColor = new Color(230, 240, 255)
    val bubbleLineColor = Color.black
    val bubbleFocusedLineColor = new Color(0, 80, 0)
    val textColor = Color.black

    val bubblePad = 5

    def connectLine(g: Graphics2D, from: Bubble, to: Bubble) {
      if (from.isFocusedParent && to.hasFocus) {
        g.setColor(new Color(100, 200, 100))
        g.setStroke(new BasicStroke(2))
        g.drawLine(from.x, from.y, to.x, to.y)
      }
      else if (to.isFocusedChild && from.hasFocus) {
        g.setColor(new Color(150, 200, 150))
        g.setStroke(new BasicStroke(2))
        g.drawLine(from.x, from.y, to.x, to.y)
      }
      else {
        g.setColor(new Color(0, 0, 0))
        g.setStroke(new BasicStroke(1))
        g.drawLine(from.x, from.y, to.x, to.y)
      }
    }

    def renderTextBubble(g: Graphics2D, focused: Boolean, text: String, x: Int, y: Int, bgColor: Color = bubbleBGColor) {
      val fm = g.getFontMetrics
      val pad = bubblePad

      val width = fm.stringWidth(text)
      val height = fm.getHeight

      val x1 = x - width/2 - pad
      val x2 = x + width/2 + pad
      val y1 = y - height/2 - pad
      val y2 = y + height/2 + pad

      g.setColor(bgColor)
      g.fillRoundRect(x1, y1, x2-x1, y2-y1, 5, 5)

      if (focused) {
        g.setColor(bubbleFocusedLineColor)
        g.setStroke(new BasicStroke(2))
        g.drawRoundRect(x1, y1, x2-x1, y2-y1, 5, 5)
      }
      else {
        g.setColor(bubbleLineColor)
        g.setStroke(new BasicStroke(1))
        g.drawRoundRect(x1, y1, x2-x1, y2-y1, 5, 5)
      }

      g.setColor(textColor)
      g.drawString(text, x-width/2, y+height/2)
    }
  }

  trait Keys { this: Editor =>
    trait SetupKeys { this: JComponent =>
      override def isFocusable = true

      var actionCounter: Int = 0

      def bind(ks: KeyStroke, action: => Unit) {
        val id = "action" + actionCounter.toString

        getInputMap.put(ks, id)
        getActionMap.put(id, new AbstractAction {
          def actionPerformed(ev: ActionEvent) {
            action
            repaint()
          }
        })

        actionCounter += 1
      }

      def bind(code: Int, modifiers: Int, action: => Unit) {
        bind(KeyStroke.getKeyStroke(code, modifiers), action)
      }

      def bind(code: Char, action: => Unit) {
        bind(KeyStroke.getKeyStroke(code), action)
      }

      // Key bindings

      {
        import KeyEvent._
        import InputEvent._

        bind(VK_UP, SHIFT_DOWN_MASK, moveCurrentGroupUp())
        bind(VK_DOWN, SHIFT_DOWN_MASK, moveCurrentGroupDown())
        bind(VK_LEFT, SHIFT_DOWN_MASK, moveCurrentGroupLeft())
        bind(VK_RIGHT, SHIFT_DOWN_MASK, moveCurrentGroupRight())

        bind(VK_UP, 0, moveToParent())
        bind(VK_DOWN, 0, moveToChild())
        bind(VK_LEFT, 0, moveToLeftBranch())
        bind(VK_RIGHT, 0, moveToRightBranch())

        bind('r', reduceCurrentNode())

        bind('+', {
          /*focusedBubble match {
            case Some(it @ BubbleContainer(m: Mystery)) =>
              replace(it, App ~ (App ~ (Plus~, Mystery~)), Mystery~)
          }*/
        })

        addMouseListener(new MouseListener {
          def mouseExited(p1: MouseEvent) {}
          def mouseClicked(p1: MouseEvent) {}
          def mouseEntered(p1: MouseEvent) {}
          def mousePressed(ev: MouseEvent) {
            // TODO: Determine if we are on a bubble.
            if (ev.isControlDown) {
              makeNewRootAtPoint(ev.getX, ev.getY)
              repaint()
            }
          }
          def mouseReleased(p1: MouseEvent) {}
        })
      }
    }
  }

  trait SomeExperiments { this: Editor =>
  }

  val editor = new Editor

  val win = new JFrame()
  win.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  win.add(editor)

  win.setSize(800, 600)
  win.setVisible(true)
}
