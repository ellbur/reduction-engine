
package reductionengine.gui
import language.postfixOps
import graphutils.GraphLayout4

trait Editing { self: Editor =>
  import sugar.{RNode, FocusedRNode}
  import sugar.{NewNode => NN, AlreadyThere => AT}
  import sugar.logic.{AlreadyThere => LAT}
  import self.{sugar => s}

  var mysteryCounter: Int = 0
  def nextMysteryCounter: Int = {
    mysteryCounter += 1
    mysteryCounter
  }
  def nextMystery: sugar.Mystery = {
    sugar.Mystery(nextMysteryCounter.toString)
  }

  def makeNewRootAtPoint(x: Int, y: Int) {
    import editingState._

    val mystery = new Mystery(Point(x, y), nextMysteryCounter.toString)

    val old = visibleBubbles.now
    val next = old + mystery

    visibleBubbles() = next
    roots() = roots.now + mystery

    focusedBubble() = Some(mystery)
    focusedChild() = None
    focusedParent() = None

    actHard(s"Make new root")
  }

  def joinTo(target: Bubble) {
    fillAHole { _ =>
      val t = AT(target)
      FocusedRNode(t, Some(t))
    }
    actHard(s"Join to tree")
  }

  def replace(at: Bubble, becomes: FocusedRNode) {
    doReplace(at, becomes)
  }

  def fillAHole(replacing: Mystery => FocusedRNode): Boolean = {
    editingState.focusedBubble.now match {
      case Some(it: Mystery) =>
        replace(it, replacing(it))
        true
      case _ =>
        message("Not a hole.")
        false
    }
  }

  def withSelected(perform: Bubble => Unit) {
    editingState.focusedBubble.now match {
      case Some(thing) => perform(thing)
      case _ => message("Nothing selected.")
    }
  }

  def doOperator(op: sugar.SugarOperator) {
    editingState.focusedBubble.now match {
      case Some(hole: Mystery) =>
        val args = 1 to op.nArgs map { i => NN(nextMystery) }
        val main = NN(s.ApicalOperator(op, args))
        val focused = if (args.length > 0) Some(args(0)) else Some(main)
        replace(hole, FocusedRNode(main, focused))
        actHard(s"Insert ${op.name}")

      case Some(thingElse) =>
        val otherArgs = 2 to op.nArgs map { i => NN(nextMystery) }
        val args = Seq(AT(thingElse)) ++ otherArgs
        val main = NN(s.ApicalOperator(op, args))
        val focused =
          if (args.length > 1)
            Some(args(1))
          else
            Some(main)
        replace(thingElse, FocusedRNode(main, focused))
        actHard(s"Insert ${op.name}")
      case None =>
        message("Nothing selected.")
    }
  }

  def insertApp() {
    if(fillAHole {_ =>
      val car, cdr = NN(nextMystery)
      FocusedRNode(
        NN(s.App(car, cdr)),
        Some(car)
      )
    })
      actHard("Insert ·")
  }

  def beginTypingNumber(`with`: Option[Char]) {
    val progress = `with` map (_.toString) getOrElse ""

    fillAHole { _ =>
      val rep = NN(sugar.NumberEditor(nextMysteryCounter.toString, progress))
      FocusedRNode(rep, Some(rep))
    }
  }

  def beginTypingVariable() {
    fillAHole { _ =>
      val rep = NN(sugar.VariableEditor(nextMysteryCounter.toString, ""))
      FocusedRNode(rep, Some(rep))
    }
  }

  def insertList() {
    fillAHole { _ =>
      actHard("[ ]")
      val element = NN(nextMystery)
      FocusedRNode(NN(sugar.PairList(element :: Nil)), Some(element))
    }
  }

  def insertElement() {
    (editingState.focusedBubble.now, editingState.focusedParent.now) match {
      case (Some(element), Some(list: PairList)) =>
        val elements = list.children.now
        val i = elements.indexOf(element)
        if (i == -1) {
          // Not much we can do.
          message("Not in a list.")
        }
        else {
          val newElement = NN(nextMystery)
          val newElements = (
               (elements.slice(0, i+1) map (AT(_)))
            ++ Seq(newElement)
            ++ (elements.slice(i+1, elements.length) map (AT(_)))
          )
          replace(list, FocusedRNode(
            NN(sugar.PairList(newElements)),
            Some(newElement)
          ))
          actHard("::")
        }
      case _ =>
        message("Not in a list.")
    }
  }

  def deleteElement() {
    (editingState.focusedBubble.now, editingState.focusedParent.now) match {
      case (Some(element), Some(list: PairList)) =>
        val elements = list.children.now
        val i = elements.indexOf(element)
        if (i == -1) {
          // Not much we can do.
          message("Not in a list.")
        }
        else {
          val newElements = (
            (elements.slice(0, i) map (AT(_)))
              ++ (elements.slice(i+1, elements.length) map (AT(_)))
            )

          val newList = NN(list.toSugarNode.now.withChildren(newElements))

          val toFocus =
            if (i > 0)
              AT(elements(i-1))
            else
              newList

          replace(list, FocusedRNode(
            newList,
            Some(toFocus)
          ))
          actHard("Delete element")
        }
      case _ =>
        message("Not in a list.")
    }
  }

  def stripMystery() {
    editingState.focusedBubble.now match {
      case None =>
        message("Nothing selected.")
      case Some(arg: Mystery) =>
        editingState.focusedParent.now match {
          case None =>
            message("No parent selected.")
          case Some(parent) =>
            val children = parent.children
            val where = children.now.indexOf(arg)
            if (where == -1) {
              message("Parent and child are not related (probably a bug).")
            }
            else {
              parent.toSugarNode.now.eliminatingChildAt(where) match {
                case Left(msg) =>
                  message(msg)
                case Right(result) =>
                  replace(parent, result.now)
                  actHard("Partially apply")
              }
            }
        }
      case Some(_) =>
        message("Not a hole.")
    }
  }

  def stripAllMysteries() {
    def stripAt(parent: Bubble) {
      val children = parent.children.now
      val toEliminate = children.zipWithIndex collect {
        case (_: Mystery, i) => i
      }
      parent.toSugarNode.now.eliminatingChildrenAt(toEliminate) match {
        case Left(msg) => message(msg)
        case Right(result) =>
          replace(parent, result)
          actHard("Partially apply")
      }
    }

    editingState.focusedBubble.now match {
      case None => message("Nothing selected")
      case Some(arg: Mystery) =>
        editingState.focusedParent.now match {
          case None => message("No parent selected")
          case Some(parent) => stripAt(parent)
        }
      case Some(notMystery) =>
        stripAt(notMystery)
    }
  }

  def clearToMystery() {
    import editingState._
    focusedBubble.now match {
      case Some(bubble) =>
        val m = NN(nextMystery)
        replace(bubble, FocusedRNode(m, Some(m)))
        actHard("Clear subtree")
      case _ =>
    }
  }

  def insertChild() {
    import editingState._

    focusedBubble.now match {
      case None =>
        message("Nothing selected.")
      case Some(bubble) =>
        replace(bubble,
          bubble.toSugarNode.now.insertingChild(sugar.AlreadyThere(bubble), NN(nextMystery))
        )
        actHard("Insert arg")
    }
  }

  def absorbParent() {
    import editingState._
    (focusedBubble.now, focusedParent.now) match {
      case (Some(bubble), Some(parent)) =>
        val it = AT(bubble)
        replace(parent, FocusedRNode(it, Some(it)))
        actHard("Absorb parent")
      case (Some(_), None) =>
        message("No parent selected")
      case (None, _) =>
        message("Nothing selected")
    }
  }

  def unroot() {
    withSelected { bubble =>
      val before = editingState.roots.now
      editingState.roots() = before.filter(_ != bubble)
    }
  }

  def label() {
    // TODO
  }

  def comment() {
    // TODO
  }

  def uncomment() {
    // TODO
  }

  def reformatSubtree() {
    currentGroup match {
      case Some(group) =>
        val groupSet = group.toSet
        reposition(editingState.visibleBubbles.now, groupSet, None)
        actHard("Reformat")
      case None =>
    }
  }

  def insertRoot(rnode: RNode, x: Int, y: Int) {
    import editingState._

    val mystery = new Mystery(Point(x, y), nextMysteryCounter.toString)

    val old = visibleBubbles.now
    val next = old + mystery

    visibleBubbles() = next
    roots() = roots.now + mystery
    replace(mystery, FocusedRNode(rnode, Some(rnode)))
  }

  def reposition(
    things: Traversable[Bubble],
    disturbed: Set[Bubble],
    center: Option[(Double, Double)]
  ) {
    val thingList = things.toIndexedSeq
    val rbIndices = thingList.zipWithIndex.toMap

    def screenCenter = (
      canvas.getX.toDouble + canvas.getWidth.toDouble/2,
      canvas.getY.toDouble + canvas.getHeight.toDouble/2
    )
    val reallyCenter = center getOrElse screenCenter

    val initial = thingList map { bubble =>
      GraphLayout4.Node(
        origPosition = {
          val loc = bubble.location.now
          Some((loc.x, loc.y))
        },
        disturb = disturbed.contains(bubble),
        adjacency = bubble.children.now flatMap (rbIndices.get(_))
      )
    }

    val computed = GraphLayout4.computeLayout(20.0, 30.0, reallyCenter, initial)
    val points = computed map { case(x, y) => Point(x.toInt, y.toInt) }

    thingList zip points foreach { case (bubble, p) =>
      bubble.location() = p
    }
  }

  def showBuryMenu() {
    openBuryChoices()
  }

  def showRecollectMenu() {
    openRecollectChoices()
  }

  def addAntiPure(idiom: sugar.KindOfIdiom, to: Bubble) {
    val editor = NN(s.AntiPureNameEditor(idiom, "", AT(to)))
    replace(to, FocusedRNode(editor, Some(editor)))
  }

  def addPure(idiom: sugar.KindOfIdiom, to: Bubble) {
    val editor = NN(s.PureNameEditor(idiom, "", AT(to)))
    replace(to, FocusedRNode(editor, Some(editor)))
  }

  def normalizeInPlace() {
    withSelected { rootBubble =>
      sugar.logic.normalize(sugar.logic.AlreadyThere(sugar.AlreadyThere(rootBubble))).now match {
        case Some(normalized) =>
          val backToSugar = sugar.SugarNode.translateDeep(normalized)
          replace(rootBubble, FocusedRNode(backToSugar, Some(backToSugar)))
          actHard("Normalize")
        case None =>
          message("Already in normal form.")
      }
    }
  }

  def normalizeCopy() {
    withSelected { rootBubble =>
      val normalized = sugar.logic.normalize(LAT(AT(rootBubble))).now match {
        case Some(normalized) => sugar.SugarNode.translateDeep(normalized)
        case None => AT(rootBubble)
      }

      val loc = rootBubble.location.now
      insertRoot(normalized, loc.x + 60, loc.y)

      actHard("Compute normal")
    }
  }

  def duplicateFully() {
    withSelected { rootBubble =>
      val dup = NN(rootBubble.toSugarNode.now.duplicated.now)
      replace(rootBubble, FocusedRNode(dup, Some(dup)))
      actHard("Duplicate subtree")
    }
  }

  def reduceCurrentNode() {
    editingState.focusedBubble.now match {
      case Some(bubble) =>
        val available = findReductionPossibilities(bubble)
        available.headOption match {
          case Some(first) =>
            val translated = sugar.SugarNode.translateDeep(first.remapping)
            replace(bubble, FocusedRNode(translated, Some(translated)))
            actHard(s"${first.name}")
          case None =>
            message("No reductions")
        }
      case None =>
        message("Nothing selected")
    }
  }
}
