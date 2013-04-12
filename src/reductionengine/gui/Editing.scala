
package reductionengine.gui
import language.postfixOps
import java.awt.Point
import scala.collection.mutable
import reactive.EventSource
import graphutils.GraphLayout4

trait Editing { self: Editor =>
  import sugar.RNode
  import sugar.{NewNode => NN, AlreadyThere => AT}
  import sugar.logic.{AlreadyThere => LAT}
  import self.{sugar => s}

  var mysteryCounter: Int = 0
  def nextMysteryCounter: Int = {
    mysteryCounter += 1
    mysteryCounter
  }
  def nextMystery: sugar.Mystery = {
    sugar.Mystery(nextMysteryCounter)
  }

  def makeNewRootAtPoint(x: Int, y: Int) {
    import editingState._

    val mystery = Mystery(nextMysteryCounter)
    val root = Root(mystery)

    val rootEditing = root.edit(x, y)
    val mysteryEditing = mystery.edit(x, y + 40)

    val old = visibleBubbles.toMap.now
    val next = old + ((root, rootEditing)) + ((mystery, mysteryEditing))

    addOrRemoveBubbles.fire(next)

    focusedBubble() = Some(EditedBubble(mystery, mysteryEditing))
    focusedChild() = None
    focusedParent() = Some(root)

    actHard(s"Make new root at ($x, $y)")
  }

  def joinTo(target: Bubble) {
    fillAHole(_ => AT(target))
    actHard(s"Join to $target")
  }

  def replace(at: Bubble, becomes: RNode) {
    doReplace.fire((at, becomes))
  }

  def fillAHole(replacing: Mystery => RNode): Boolean = {
    editingState.focusedBubble.now match {
      case Some(EditedBubble(it: Mystery, _)) =>
        replace(it, replacing(it))
        true
      case _ =>
        message("Not a hole.")
        false
    }
  }

  def withSelected(perform: EditedBubble => ResultingDescription): ResultingDescription = {
    editingState.focusedBubble.now match {
      case Some(thing) => perform(thing)
      case _ =>
        message("Nothing selected.")
        NotPerformed
    }
  }

  def doOperator(op: sugar.SugarOperator): Action = {
    Action(s"Insert $op", Hard) {
      fillAHole(_ =>
        NN(sugar.ApicalOperator(op, 1 to op.nArgs map { i =>
          NN(
            if (i == 1)
              sugar.Focused(NN(nextMystery))
            else
              nextMystery
          )
        })
      )) match {
        case true =>
          AsDescribed
        case false =>
          NotPerformed
      }
    }
  }

  val insertApp = Action("Insert app", Hard) {
    fillAHole(_ =>
      NN(sugar.App(NN(sugar.Focused(NN(nextMystery))), NN(nextMystery)))
    ) match {
      case true => AsDescribed
      case false => NotPerformed
    }
  }

  def beginTypingNumber(`with`: Option[Char]) {
    val progress = `with` map (_.toString) getOrElse ""

    fillAHole(_ => NN(
      sugar.Focused(NN(sugar.NumberEditor(nextMysteryCounter, progress)))
    ))
  }

  val beginTypingVariable = Action("Insert variable", Nada) {
    /*
    fillAHole(_ =>
      NN(sugar.VariableEditor(nextMysteryCounter, ""))
    ) match {
      case true => AsDescribed
      case false => NotPerformed
    }
    */
    // TODO
    message("Not implemented.")
    NotPerformed
  }

  val stripMystery = Action("Partially apply", Hard) {
    editingState.focusedBubble.now match {
      case None =>
        message("Nothing selected.")
        NotPerformed
      case Some(EditedBubble(arg: Mystery, _)) =>
        editingState.focusedParent.now match {
          case None =>
            message("No parent selected.")
            NotPerformed
          case Some(parent) =>
            val children = parent.children
            val where = children.indexOf(arg)
            if (where == -1) {
              message("Parent and child are not related (probably a bug).")
              NotPerformed
            }
            else {
              parent.toSugarNode.eliminatingChildAt(where) match {
                case Left(msg) =>
                  message(msg)
                case Right(result) =>
                  replace(parent, result)
              }
              AsDescribed
            }
        }
      case Some(_) =>
        message("Not a hole.")
        NotPerformed
    }
  }

  val clearToMystery = Action("Clear to hole", Hard) {
    import editingState._
    focusedBubble.now match {
      case Some(bubble) =>
        doReplace.fire((bubble.bubble, NN(nextMystery)))
        AsDescribed
      case _ =>
        NotPerformed
    }
  }

  val insertChild = Action("Insert argument", Hard) {
    import editingState._

    focusedBubble.now match {
      case None =>
        message("Nothing selected.")
        NotPerformed
      case Some(EditedBubble(bubble, _)) =>
        replace(bubble,
          bubble.toSugarNode.insertingChild(sugar.AlreadyThere(bubble), NN(nextMystery))
        )
        AsDescribed
    }
  }

  val reformatSubtree = Action("Reformat subtree", Hard) {
    currentGroup match {
      case Some(group) =>
        val groupSet = group.toSet

        val before = editingState.visibleBubbles.toMap.now.toTraversable map {
          case (bubble, editing) =>
            (bubble, Some(editing))
        }
        val after = reposition(before, groupSet, None)
        editingState.addOrRemoveBubbles.fire(after map {
          case EditedBubble(bubble, editing) => (bubble, editing)
        } toMap)
        AsDescribed
      case None =>
        NotPerformed
    }
  }

  def insertRoot(rnode: RNode, x: Int, y: Int) {
    import editingState._

    val mystery = Mystery(nextMysteryCounter)
    val root = Root(mystery)

    val rootEditing = root.edit(x, y)
    val mysteryEditing = mystery.edit(x, y + 40)

    val old = visibleBubbles.toMap.now
    val next = old + ((root, rootEditing)) + ((mystery, mysteryEditing))

    addOrRemoveBubbles.fire(next)

    replace(mystery, rnode)
  }

  def reposition(
    things: Traversable[(Bubble, Option[BubbleEditing])],
    disturbed: Set[Bubble],
    center: Option[(Double, Double)]
  ): Traversable[EditedBubble] =
  {
    val thingList = things.toIndexedSeq
    val bubbleIndices = thingList.map(_._1).zipWithIndex.toMap

    def screenCenter = (
      canvas.getX.toDouble + canvas.getWidth.toDouble/2,
      canvas.getY.toDouble + canvas.getHeight.toDouble/2
    )
    val reallyCenter = center getOrElse screenCenter

    val initial = thingList map { case (bubble, optionalEditing) =>
      GraphLayout4.Node(
        origPosition = optionalEditing map (editing =>
          (editing.x.now.toDouble, editing.y.now.toDouble)
          ),
        disturb = disturbed contains bubble,
        adjacency = bubble.children flatMap (bubbleIndices.get(_))
      )
    }

    val coordinates = GraphLayout4.computeLayout(
      40.0,
      reallyCenter,
      initial
    )

    (thingList.zipWithIndex map { case ((bubble, optionalEditing), index) =>
      val (newX, newY) = coordinates(index)
      EditedBubble(bubble, optionalEditing match {
        case Some(editing) =>
          editing.x() = newX.toInt
          editing.y() = newY.toInt
          editing
        case None =>
          bubble.edit(newX.toInt, newY.toInt)
      })
    })
  }

  val showBuryMenu = Action("Bury", Nada) {
    wantsToBury.fire(())
    AsDescribed
  }

  val showRecollectMenu = Action("Recollect", Nada) {
    wantsToRecollect.fire(())
    AsDescribed
  }

  def addAntiPure(idiom: sugar.KindOfIdiom, to: Bubble) {
    replace(to, NN(s.Focused(
      NN(s.AntiPureNameEditor(idiom, "", AT(to)))
    )))
  }

  def addPure(idiom: sugar.KindOfIdiom, to: Bubble) {
    replace(to, NN(s.Focused(
      NN(s.PureNameEditor(idiom, "", AT(to)))
    )))
  }

  val normalizeInPlace = Action("Normalize", Hard) {
    withSelected { rootBubble =>
      sugar.logic.normalize(sugar.logic.AlreadyThere(rootBubble.bubble)) match {
        case Some(normalized) =>
          val backToSugar = sugar.SugarNode.translateDeep(normalized)
          replace(rootBubble.bubble, backToSugar)
          AsDescribed
        case None =>
          message("Already in normal form.")
          NotPerformed
      }
    }
  }

  val normalizeCopy = Action("Compute Normal", Hard) {
    withSelected { rootBubble =>
      val normalized = sugar.logic.normalize(LAT(rootBubble.bubble)) match {
        case Some(normalized) => sugar.SugarNode.translateDeep(normalized)
        case None => AT(rootBubble.bubble)
      }

      insertRoot(normalized, rootBubble.editing.x.now + 60, rootBubble.editing.y.now)

      AsDescribed
    }
  }

  val duplicateFully = Action("Duplicate Subtree", Hard) {
    withSelected { rootBubble =>
      replace(rootBubble.bubble, NN(rootBubble.bubble.toSugarNode.duplicated))
      AsDescribed
    }
  }
}
