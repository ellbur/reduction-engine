
package reductionengine.sugar

trait SugarNodes { self: Idioms =>
  type NodeType <: logic.NodeLike with SugarNodeLike

  trait SugarNodeLike {
    val toSugarNode: SugarNode
  }

  type RNode = SugarReplacement
  import self.{ NewNode => SNN }

  object logic extends reductionengine.logic.Logic {
    type IdiomType = self.Idiom
    type NodeType = self.NodeType
  }
  import logic.{NewNode => NN, AlreadyThere => AT, applyAll, applyAllTo}

  sealed trait SugarNode {
    val toNode: logic.Node
    val children: Seq[RNode]
    def withChildren(children: Seq[RNode]): SugarNode
    def eliminatingChildAt(index: Int): Either[String, RNode] =
      Left("Not support for this kind of node.")
    def insertingChild(as: RNode, child: RNode): RNode =
      SNN(App(as, SNN(Focused(child))))
    def duplicated: SugarNode = withChildren(children map (_.duplicated))
  }
  object SugarNode {
    def translateDeep(n: logic.RNode): RNode = {
      import SugarNode.{translateDeep => t}

      n match {
        case logic.AlreadyThere(it)  => AlreadyThere(it)
        case logic.NewNode(node)     => NewNode(node match {
          case logic.App(car, cdr)         => App(t(car), t(cdr))
          case logic.Pure(idiom, expr)     => Pure(idiom.rep, t(expr))
          case logic.AntiPure(idiom, expr) => AntiPure(idiom.rep, t(expr))
          case logic.IntLiteral(n)         => IntLiteral(n)
          case logic.OperatorLiteral(op)   => ApicalOperator(BasicOperator(op), Seq())
          case logic.Mystery(n)            => Mystery(n)
        })
      }
    }
  }

  case class IntLiteral(n: Int) extends SugarNode {
    lazy val toNode = logic.IntLiteral(n)
    lazy val children = Seq()
    def withChildren(children: Seq[RNode]) = this
  }

  sealed abstract class SugarOperator(val name: String, val nArgs: Int)
  case class BasicOperator(op: logic.Operator) extends SugarOperator(op.name, op.nArgs) {
    override def toString = op.toString
  }
  case object B extends SugarOperator("B", 2)

  case class ApicalOperator(op: SugarOperator, args: Seq[RNode])
    extends SugarNode
  {
    lazy val toNode = {
      op match {
        case B =>
          applyAll(args map (_.toNode)) match {
            case NN(it) => it
            case it @ AT(_) =>
              logic.App(NN(logic.OperatorLiteral(logic.I)), it)
          }
        case BasicOperator(op) =>
          val theArgs = args map (_.toNode)
          val result = applyAllTo(logic.OperatorLiteral(op), args map (_.toNode))
          result
      }
    }
    lazy val children = args.toSeq
    def withChildren(children: Seq[RNode]) = copy(args = children)

    // We must assume here that this is in fact a valid child index.
    override def eliminatingChildAt(index: Int): Either[String, RNode] = Right {
      if (index == args.length-1) {
        val nextArgs = args.init
        nextArgs.reverse.toList match {
          case Nil => SNN(Focused(SNN(ApicalOperator(op, Seq()))))
          case last :: before =>
            val thenTheyAre = (SNN(Focused(last)) :: before).reverse
            SNN(ApicalOperator(op, thenTheyAre))
        }
      }
      else {
        val idiom = (Stream.from(0) map {
          case 0 => "_"
          case n => s"_$n"
        } map { name =>
          Idiom(standardIdiomKinds.lambda, name)
        } filter { idiom =>
          this.toNode.isNotImpureIn(idiom.toLogic)
        }).head

        val I = ApicalOperator(BasicOperator(logic.I), Seq())
        val nextArgs = args.zipWithIndex map {
          case (arg, i) =>
            if (i == index)
              SNN(AntiPure(idiom, SNN(I)))
            else
              arg
        }
        SNN(Pure(idiom, SNN(ApicalOperator(op, nextArgs))))
      }
    }

    override def insertingChild(as: RNode, child: RNode) =
      if (args.length < op.nArgs)
        SNN(ApicalOperator(op, args :+ SNN(Focused(child))))
      else
        SNN(App(as, SNN(Focused(child))))
  }

  object App {
    def apply(car: RNode, cdr: RNode): ApicalOperator =
      ApicalOperator(B, Seq(car, cdr))
  }

  case class Pure(idiom: Idiom, is: RNode) extends SugarNode {
    lazy val toNode = logic.Pure(idiom.toLogic, is.toNode)
    lazy val children = Seq(is)
    def withChildren(children: Seq[RNode]) = children match { case Seq(c) => copy(is=c) }
  }

  case class AntiPure(idiom: Idiom, is: RNode) extends SugarNode {
    lazy val toNode = logic.AntiPure(idiom.toLogic, is.toNode)
    lazy val children = Seq(is)
    def withChildren(children: Seq[RNode]) = children match { case Seq(c) => copy(is=c) }
  }

  case class Root(is: RNode) extends SugarNode {
    lazy val toNode = logic.Mystery(0)
    lazy val children = Seq(is)
    def withChildren(children: Seq[RNode]) = children match { case Seq(c) => copy(is=c) }
  }

  case class Mystery(id: Int) extends SugarNode {
    lazy val toNode = logic.Mystery(id)
    lazy val children = Seq()
    def withChildren(children: Seq[RNode]) = this
  }

  case class Open(id: String) extends SugarNode {
    lazy val toNode = logic.Mystery(0)
    lazy val children = Seq()
    override lazy val toString = id
    def withChildren(children: Seq[RNode]) = this
  }

  case class NumberEditor(id: Int, progress: String) extends SugarNode {
    lazy val toNode = logic.Mystery(id)
    lazy val children = Seq()
    def withChildren(children: Seq[RNode]) = this
  }

  case class AntiPureNameEditor(idiomKind: KindOfIdiom, progress: String, of: RNode) extends SugarNode {
    lazy val toNode = logic.Mystery(0)
    lazy val children = Seq(of)
    def withChildren(children: Seq[RNode]) = children match { case Seq(c) => copy(of=c) }
  }

  case class PureNameEditor(idiomKind: KindOfIdiom, progress: String, of: RNode) extends SugarNode {
    lazy val toNode = logic.Mystery(0)
    lazy val children = Seq(of)
    def withChildren(children: Seq[RNode]) = children match { case Seq(c) => copy(of=c) }
  }

  case class Focused(is: RNode) extends SugarNode {
    lazy val toNode = is match {
      case AlreadyThere(it) => it.toNode
      case NewNode(node, _) => node.toNode
    }
    lazy val children = Seq(is)
    def withChildren(children: Seq[RNode]) = children match { case Seq(c) => copy(is=c) }
  }

  sealed trait SugarReplacement {
    val height: Int
    def apply(cdr: SugarReplacement) = NewNode(App(this, cdr))
    val toNode: logic.RNode
    def duplicated: RNode
  }
  case class AlreadyThere(t: NodeType) extends SugarReplacement {
    lazy val height = 0
    override lazy val toString = "*"
    val toNode = logic.AlreadyThere(t)
    def duplicated = NewNode(t.toSugarNode.duplicated, replacing=Some(this))
  }
  case class NewNode(s: SugarNode, replacing: Option[RNode] = None) extends SugarReplacement {
    lazy val height = {
      val ch = s.children map (_.height)
      if (ch.isEmpty)
        0
      else
        1 + ch.max
    }
    override lazy val toString = s.toString
    val toNode = NN(s.toNode)
    def duplicated = NewNode(s.duplicated, replacing=Some(this))
  }
}
