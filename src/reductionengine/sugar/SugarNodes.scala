
package reductionengine.sugar

trait SugarNodes { self: Idioms =>
  type NodeType
  val nodeLike: logic.NodeLike

  type RNode = SugarReplacement

  def toNode(r: RNode): logic.RNode = r match {
    case AlreadyThere(it) => logic.AlreadyThere(it)
    case NewNode(it) => logic.NewNode(it.toNode)
  }

  object logic extends reductionengine.logic.Logic {
    type IdiomType = self.Idiom
    type NodeType = self.NodeType
    val nodeLike = self.nodeLike
  }
  import logic.{NewNode => NN, AlreadyThere => AT, applyAll, applyAllTo}

  sealed trait SugarNode {
    val toNode: logic.Node
    val children: Traversable[RNode]
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
  }

  sealed abstract class SugarOperator(val name: String, val nArgs: Int)
  case class BasicOperator(op: logic.Operator) extends SugarOperator(op.name, op.nArgs)
  case object B extends SugarOperator("B", 2)

  case class ApicalOperator(op: SugarOperator, args: Seq[RNode])
    extends SugarNode
  {
    lazy val toNode = {
      println(3)
      op match {
        case B =>
          applyAll(args map (self.toNode _)) match {
            case NN(it) => it
            case it @ AT(_) =>
              logic.App(NN(logic.OperatorLiteral(logic.I)), it)
          }
        case BasicOperator(op) =>
          val theArgs = args map (self.toNode(_))
          val result = applyAllTo(logic.OperatorLiteral(op), args map (self.toNode _))
          result
      }
    }
    lazy val children = args.toSeq
  }

  object App {
    def apply(car: RNode, cdr: RNode): ApicalOperator =
      ApicalOperator(B, Seq(car, cdr))
  }

  case class Pure(idiom: Idiom, is: RNode) extends SugarNode {
    lazy val toNode = logic.Pure(idiom.toLogic, self.toNode(is))
    lazy val children = Seq(is)
  }

  case class AntiPure(idiom: Idiom, is: RNode) extends SugarNode {
    lazy val toNode = logic.AntiPure(idiom.toLogic, self.toNode(is))
    lazy val children = Seq(is)
  }

  case class Root(is: RNode) extends SugarNode {
    lazy val toNode = logic.Mystery(0)
    lazy val children = Seq(is)
  }

  case class Mystery(id: Int) extends SugarNode {
    lazy val toNode = logic.Mystery(id)
    lazy val children = Seq()
  }

  case class Open(id: String) extends SugarNode {
    lazy val toNode = logic.Mystery(0)
    lazy val children = Seq()
    override lazy val toString = id
  }

  case class NumberEditor(id: Int, progress: String) extends SugarNode {
    lazy val toNode = logic.Mystery(id)
    lazy val children = Seq()
  }

  case class AntiPureNameEditor(idiomKind: KindOfIdiom, progress: String, of: RNode) extends SugarNode {
    lazy val toNode = logic.Mystery(0)
    lazy val children = Seq(of)
  }

  case class PureNameEditor(idiomKind: KindOfIdiom, progress: String, of: RNode) extends SugarNode {
    lazy val toNode = logic.Mystery(0)
    lazy val children = Seq(of)
  }

  case class Focused(is: RNode) extends SugarNode {
    lazy val toNode = is match {
      case AlreadyThere(it) => nodeLike.toNode(it)
      case NewNode(node) => node.toNode
    }
    lazy val children = Seq(is)
  }

  sealed trait SugarReplacement {
    val height: Int
    def apply(cdr: SugarReplacement) = NewNode(App(this, cdr))
  }
  case class AlreadyThere(t: NodeType) extends SugarReplacement {
    lazy val height = 0
    override lazy val toString = "*"
  }
  case class NewNode(s: SugarNode) extends SugarReplacement {
    lazy val height = {
      val ch = s.children map (_.height)
      if (ch.isEmpty)
        0
      else
        1 + ch.max
    }
    override lazy val toString = s.toString
  }
}
