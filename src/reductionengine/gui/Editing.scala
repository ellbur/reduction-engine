
package reductionengine.gui
import language.postfixOps
import reductionengine.{logic, sugar}
import sugar.{NewNode => NN, AlreadyThere => AT, SugarReplacement, SugarNode}
import sugar.SugarNode.{sugarReplacementNodeLike, noNeed}

trait Editing { this: Editor =>
  var mysteryCounter: Int = 0
  def nextMysteryCounter: Int = {
    mysteryCounter += 1
    mysteryCounter
  }
  def nextMystery: sugar.Mystery = {
    sugar.Mystery(nextMysteryCounter)
  }

  def makeNewRootAtPoint(x: Int, y: Int) {
    val mystery = Mystery(nextMysteryCounter, x + 50, y + 10)
    val it = Root(mystery, x, y)
    bubbles ++= Seq(it, mystery)
    roots += it

    focusedBubble = Some(mystery)
    focusedChild = None
    focusedParent = Some(it)
  }

  def fillAHole(replacing: Mystery => SugarReplacement[Bubble]) {
    focusedBubble match {
      case Some(it: Mystery) =>
        replace(it, replacing(it))
      case _ =>
        message("Not a hole.")
    }
  }

  def doOperator(op: sugar.SugarOperator) {
    fillAHole(_ =>
      NN(sugar.ApicalOperator(op, 1 to op.nArgs map { i =>
        NN(
          if (i == 1)
            sugar.Focused(nextMystery)
          else
            nextMystery
        )
      })
    ))
  }

  def doApp() {
    fillAHole(_ =>
      NN(sugar.App(NN(sugar.Focused(nextMystery)), NN(nextMystery)))
    )
  }

  def beginTypingNumber(`with`: Option[Char]) {
    val progress = `with` map (_.toString) getOrElse ""

    fillAHole(_ => NN(
      sugar.Focused(sugar.NumberEditor(nextMysteryCounter, progress))
    ))
  }

  def stripMystery() {
    focusedBubble match {
      case Some(it: Mystery) =>
        for (b <- bubbles)
          for (e <- b.childEdges if e.target==it) {
            e.deleteIfReasonable()
          }
        runGC()
        updateBubblyThings()
      case Some(_) =>
        message("Not a hole.")
      case None =>
        message("Nothing selected.")
    }
  }

  def clearToMystery() {
    focusedBubble match {
      case Some(it) =>
        replace(it, NN(nextMystery))
      case _ =>
        message("Nothing selected.")
    }
  }

  def insertChild() {
    focusedBubble match {
      case Some(it: ApicalOperator) =>
        if (it.childBubbles.length < it.op.nArgs) {
          val cxs = (it.children map (_.x)).toSeq
          val cys = (it.children map (_.y)).toSeq

          val x =
            if (cxs.length == 0)
              it.x
            else cxs.max + 20

          val y =
            if (cys.length == 0)
              it.y + 30
            else
              (cys.sum) / cys.length

          val ch = Mystery(nextMysteryCounter, x, y)
          bubbles += ch
          it.childBubbles += ch
          focusedBubble = Some(ch)
          focusedParent = Some(it)
          focusedChild = None
          reposition(it.children.toSeq)
        }
        else {
          replace(it, NN(sugar.App(AT(it), NN(nextMystery))))
        }
      case Some(it) =>
        replace(it, NN(sugar.App(AT(it), NN(nextMystery))))
      case None =>
        message("Nothing selected.")
    }
  }

  def reformatSubtree() {
    currentGroup match {
      case Some(group) =>
        reposition(group.toSeq)
      case None =>
        message("No group selected.")
    }
  }
}
