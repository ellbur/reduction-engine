
package reductionengine.gui

import java.awt.Graphics2D
import collection.mutable.ArrayBuffer
import javax.swing.JTextField
import java.awt.event.{ActionEvent, ActionListener}

trait KindsOfBubbles { this: Editor =>
  import sugar.{ NewNode => NN, AlreadyThere => AT }

  class IntLiteral(
    val n: Int) extends Bubble
  {
    val childEdges = Seq()
    lazy val toSugarNode = sugar.IntLiteral(n)
    override def toString = n.toString
    def withChildren(children: Seq[Bubble]) = this
    def edit(x: Int, y: Int) = new IntLiteralEditing(x, y, n)
  }
  object IntLiteral {
    def apply(n: Int) = new IntLiteral(n)
  }

  class ApicalOperator(val op: sugar.SugarOperator, val childBubbles: Seq[Bubble])
    extends Bubble
  { thisBubble =>
    lazy val childEdges = childBubbles map (basicEdge(_))
    lazy val toSugarNode = sugar.ApicalOperator(op, (childBubbles map (sugar.AlreadyThere(_))).toSeq)
    def withChildren(children: Seq[Bubble]) = new ApicalOperator(op, children)
    override def toString = s"$op(${childBubbles map (_.toString) mkString ","})"
    def edit(x: Int, y: Int) = new ApicalOperatorEditing(x, y, op)
  }
  object ApicalOperator {
    def apply(op: sugar.SugarOperator, children: Seq[Bubble]) = new ApicalOperator(op, children)
  }

  class Pure(val idiom: sugar.Idiom, val is: Bubble) extends Bubble {
    lazy val childEdges = Seq(basicEdge(is))
    lazy val toSugarNode = sugar.Pure(idiom, sugar.AlreadyThere(is))
    def withChildren(children: Seq[Bubble]) = children match {
      case Seq(child) => new Pure(idiom, child)
    }
    def edit(x: Int, y: Int) = new PureEditing(x, y, idiom)
  }
  object Pure {
    def apply(idiom: sugar.Idiom, is: Bubble) = new Pure(idiom, is)
  }

  class AntiPure(val idiom: sugar.Idiom, val is: Bubble) extends Bubble {
    lazy val childEdges = Seq(basicEdge(is))
    lazy val toSugarNode = sugar.AntiPure(idiom, sugar.AlreadyThere(is))
    def withChildren(children: Seq[Bubble]) = children match {
      case Seq(child) => new AntiPure(idiom, child)
    }
    def edit(x: Int, y: Int) = new AntiPureEditing(x, y, idiom)
  }
  object AntiPure {
    def apply(idiom: sugar.Idiom, is: Bubble) = new AntiPure(idiom, is)
  }

  class Root(val is: Bubble) extends Bubble
  {
    lazy val childEdges = Seq(basicEdge(is))
    lazy val toSugarNode = sugar.Root(sugar.AlreadyThere(is))
    def withChildren(children: Seq[Bubble]) = children match {
      case Seq(child) => new Root(child)
    }
    override def toString = s"*$is"
    def edit(x: Int, y: Int) = new RootEditing(x, y)
  }
  object Root {
    def apply(is: Bubble) = new Root(is)
  }

  class Mystery(val n: Int) extends Bubble
  {
    lazy val childEdges = Seq()
    lazy val toSugarNode = sugar.Mystery(n)
    def withChildren(children: Seq[Bubble]) = this
    override def toString = s"?$n"
    def edit(x: Int, y: Int) = new MysteryEditing(x, y)
  }
  object Mystery {
    def apply(n: Int) = new Mystery(n)
  }

  class NumberEditor(id: Int, val initially: String)
    extends Bubble
  {
    val childEdges = Seq()
    def withChildren(children: Seq[Bubble]) = this
    lazy val toSugarNode = sugar.NumberEditor(id, "")
    def edit(x: Int, y: Int) = new NumberEditorEditing(x, y, initially)
  }
  object NumberEditor {
    def apply(id: Int, initially: String) = new NumberEditor(id, initially)
  }

  class AntiPureNameEditor(val idiomKind: sugar.KindOfIdiom, val initially: String, val of: Bubble)
    extends Bubble
  {
    lazy val childEdges = Seq(basicEdge(of))
    def withChildren(children: Seq[Bubble]) = children match {
      case Seq(child) => new AntiPureNameEditor(idiomKind, initially, child)
    }
    lazy val toSugarNode = sugar.AntiPureNameEditor(idiomKind, "", AT(of))

    def edit(x: Int, y: Int) = new AntiPureNameEditorEditing(x, y, idiomKind, initially)
  }
  object AntiPureNameEditor {
    def apply(idiomKind: sugar.KindOfIdiom, initially: String, of: Bubble) =
      new AntiPureNameEditor(idiomKind, initially, of)
  }

  class PureNameEditor(val idiomKind: sugar.KindOfIdiom, val initially: String, val of: Bubble)
    extends Bubble
  {
    lazy val initialText = initially
    lazy val childEdges = Seq(basicEdge(of))
    def withChildren(children: Seq[Bubble]) = children match {
      case Seq(child) => new PureNameEditor(idiomKind, initially, child)
    }
    lazy val toSugarNode = sugar.PureNameEditor(idiomKind, "", AT(of))

    def edit(x: Int, y: Int) = new PureNameEditorEditing(x, y, idiomKind, initially)
  }
  object PureNameEditor {
    def apply(idiomKind: sugar.KindOfIdiom, initially: String, of: Bubble) =
      new PureNameEditor(idiomKind, initially, of)
  }

  class VariableEditor(val id: Int, initially: String) extends Bubble {
    lazy val childEdges = Seq()
    def withChildren(children: Seq[Bubble]) = this
    lazy val toSugarNode = sugar.VariableEditor(id, initially)
    def edit(x: Int, y: Int) = new VariableEditorEditing(x, y, initially)
  }
  object VariableEditor {
    def apply(id: Int, initially: String) = new VariableEditor(id, initially)
  }

  class Variable(name: String) extends Bubble {
    lazy val childEdges = Seq()
    def withChildren(children: Seq[Bubble]) = this
    lazy val toSugarNode = sugar.Variable(name)
    def edit(x: Int, y: Int) = new VariableEditing(x, y, name)
  }
  object Variable {
    def apply(name: String) = new Variable(name)
  }
}
