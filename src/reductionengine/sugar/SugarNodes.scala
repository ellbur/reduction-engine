

package reductionengine.sugar

import scalaz._
import Scalaz._

trait SugarNodes { self: Idioms =>
  type M[+X]
  def now[A](x: M[A]): A
  implicit val monad: Monad[M]
  type NodeType <: SugarNodeLike

  implicit class Now[A](x: M[A]) {
    def now = self.now(x)
  }

  trait SugarNodeLike extends logic.NodeLike {
    val toSugarNode: M[SugarNode]
    lazy val toNode: M[logic.Node] = toSugarNode map (_.toNode)
  }

  type RNode = SugarReplacement
  import self.{ NewNode => SNN }

  object logic extends reductionengine.logic.Logic {
    type M[+X] = self.M[X]
    val monad = self.monad
    type IdiomType = self.Idiom
    type NodeType = self.NodeType
  }
  import logic.{NewNode => NN, AlreadyThere => AT, applyAll, applyAllTo}

  case class FocusedRNode(rnode: RNode, focus: Option[RNode])

  sealed trait SugarNode {
    val toNode: logic.Node
    val children: Seq[RNode]
    def withChildren(children: Seq[RNode]) = local.manifest(children)
    def eliminatingChildAt(index: Int): Either[String, M[FocusedRNode]] =
      Left("Not supported for this kind of node.")
    def eliminatingChildrenAt(indices: Traversable[Int]): Either[String, FocusedRNode] =
      Left("Not supported for this kind of node.")
    def insertingChild(as: RNode, child: RNode): FocusedRNode =
      FocusedRNode(SNN(App(as, child)), Some(child))

    lazy val duplicated: M[SugarNode] = {
      val dupChildren = (children map (_.duplicated)).sequence
      dupChildren map { dupChildren =>
        withChildren(dupChildren)
      }
    }
    val local: LocalNode
  }
  object SugarNode {
    def translateDeep(n: logic.RNode): RNode = {
      import SugarNode.{translateDeep => t}

      n match {
        case logic.AlreadyThere(it)  => AlreadyThere(it)
        case logic.NewNode(node)     => NewNode(node match {
          case logic.App(car, cdr)         => App(t(car), t(cdr))
          case logic.Pure(idiom, is)       => Pure(idiom.rep, t(is))
          case logic.AntiPure(idiom)       => AntiPure(idiom.rep)
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
    lazy val local = LocalNode.IntLiteral(n)
  }

  sealed abstract class SugarOperator(val name: String, val nArgs: Int)
  case class BasicOperator(op: logic.Operator) extends SugarOperator(op.name, op.nArgs) {
    override def toString = op.toString
  }
  case object B extends SugarOperator("B", 2)
  case class AntiPureOp(idiom: Idiom) extends SugarOperator(idiom.toString, 1)

  case class ApicalOperator(op: SugarOperator, args: Seq[RNode])
    extends SugarNode
  {
    lazy val toNode = {
      op match {
        case BasicOperator(op) =>
          val theArgs = args map (_.toNode)
          val result = applyAllTo(logic.OperatorLiteral(op), args map (_.toNode))
          result
        case B =>
          applyAll(args map (_.toNode)) match {
            case NN(it) => it
            case it @ AT(_) =>
              logic.App(NN(logic.OperatorLiteral(logic.I)), it)
          }
        case AntiPureOp(idiom) =>
          val theArgs = args map (_.toNode)
          val result = applyAllTo(logic.AntiPure(idiom.toLogic), args map (_.toNode))
          result
      }
    }
    lazy val children = args.toSeq
    lazy val local = LocalNode.ApicalOperator(op)

    // We must assume here that this is in fact a valid child index.
    override def eliminatingChildAt(index: Int): Either[String, M[FocusedRNode]] = Right {
      if (index == args.length-1) {
        val nextArgs = args.init
        nextArgs.reverse.toList match {
          case Nil =>
            val opn = SNN(ApicalOperator(op, Seq()))
            FocusedRNode(
              opn, Some(opn)
            ).pure
          case last :: before =>
            val thenTheyAre = (last :: before).reverse
            FocusedRNode(
              SNN(ApicalOperator(op, thenTheyAre)),
              Some(last)
            ).pure
        }
      }
      else {
        def findAcceptableName(n: Int): M[Idiom] = {
          val idiom = Idiom(standardIdiomKinds.lambda, n match {
            case 0 => "_"
            case n => s"_$n"
          })

          toNode.isNotImpureIn(idiom.toLogic) flatMap {
            case true => idiom.pure
            case false => findAcceptableName(n + 1)
          }
        }
        findAcceptableName(0) map { idiom =>
          val I = ApicalOperator(BasicOperator(logic.I), Seq())
          val nextArgs = args.zipWithIndex map {
            case (arg, i) =>
              if (i == index)
                SNN(AntiPure(idiom, SNN(I)))
              else
                arg
          }
          val troot = SNN(Pure(idiom, SNN(ApicalOperator(op, nextArgs))))
          FocusedRNode(
            troot,
            Some(troot)
          )
        }
      }
    }

    override def eliminatingChildrenAt(indices: Traversable[Int]): Either[String, FocusedRNode] = {
      val consider = indices.toSeq.sorted.reverse.toList

      def transform(children: Seq[RNode], above: RNode => RNode, consider: List[Int]): FocusedRNode = consider match {
        case Nil =>
          val spot = SNN(this.withChildren(children))
          FocusedRNode(above(spot), Some(spot))
        case index :: indices =>
          if (index == children.length-1)
            transform(children.init, above, indices)
          else {
            def findAcceptableName(n: Int): (String, Idiom) = {
              val name = n match {
                case 0 => "_"
                case n => s"_$n"
              }
              val idiom = Idiom(standardIdiomKinds.lambda, name)

              children.all { child =>
                child.toNode.toNode.now.isNotImpureIn(idiom.toLogic).now
              } match {
                case true => (name, idiom)
                case false => findAcceptableName(n + 1)
              }
            }
            val (name, idiom) = findAcceptableName(0)
            val nextChildren = children.updated(index, SNN(Variable(name)))
            val nextAbove = { (inner: RNode) =>
              SNN(Pure(idiom, above(inner)))
            }

            transform(nextChildren, nextAbove, indices)
          }
      }

      Right(transform(children, identity, consider))
    }

    override def insertingChild(as: RNode, child: RNode) =
      if (args.length < op.nArgs)
        FocusedRNode(
          SNN(ApicalOperator(op, args :+ child)),
          Some(child)
        )
      else
        FocusedRNode(
          SNN(App(as, child)),
          Some(child)
        )
  }

  object App {
    def apply(car: RNode, cdr: RNode): ApicalOperator =
      ApicalOperator(B, Seq(car, cdr))
  }

  object AntiPure {
    def apply(idiom: Idiom) = ApicalOperator(AntiPureOp(idiom), Seq())
    def apply(idiom: Idiom, is: RNode) = ApicalOperator(AntiPureOp(idiom), Seq(is))
  }

  case class Pure(of: Idiom, is: RNode) extends SugarNode {
    lazy val toNode = logic.Pure(of.toLogic, is.toNode)
    lazy val children = Seq(is)
    lazy val local = LocalNode.Pure(of)
  }

  case class Mystery(name: String) extends SugarNode {
    lazy val toNode = logic.Mystery(name)
    lazy val children = Seq()
    lazy val local = LocalNode.Mystery(name)
  }

  case class NumberEditor(name: String, progress: String) extends SugarNode {
    lazy val toNode = logic.Mystery(name)
    lazy val children = Seq()
    lazy val local = LocalNode.NumberEditor(name, progress)
  }

  case class AntiPureNameEditor(idiomKind: KindOfIdiom, progress: String, of: RNode) extends SugarNode {
    lazy val toNode = logic.Mystery(idiomKind.toString)
    lazy val children = Seq(of)
    lazy val local = LocalNode.AntiPureNameEditor(idiomKind, progress)
  }

  case class PureNameEditor(idiomKind: KindOfIdiom, progress: String, of: RNode) extends SugarNode {
    lazy val toNode = logic.Mystery(idiomKind.toString)
    lazy val children = Seq(of)
    lazy val local = LocalNode.PureNameEditor(idiomKind, progress)
  }

  /** Name is not the name of the variable; it identifies the editor. */
  case class VariableEditor(name: String, progress: String) extends SugarNode {
    lazy val toNode = logic.Mystery(name)
    lazy val children = Seq()
    lazy val local = LocalNode.VariableEditor(name, progress)
  }

  case class Variable(name: String) extends SugarNode {
    lazy val toNode = logic.App(
      logic.NewNode(logic.AntiPure(Idiom(standardIdiomKinds.lambda, name).toLogic)),
      standardCombinators.I.toNode
    )
    lazy val children = Seq()
    lazy val local = LocalNode.Variable(name)
  }

  sealed trait SugarReplacement {
    val height: Int
    def apply(cdr: SugarReplacement) = NewNode(App(this, cdr))
    val toNode: logic.RNode
    val duplicated: M[RNode]
    val children: M[Seq[RNode]]
  }
  case class AlreadyThere(t: NodeType) extends SugarReplacement {
    lazy val height = 0
    override lazy val toString = "*"
    val toNode = logic.AlreadyThere(t)
    lazy val duplicated = t.toSugarNode flatMap (_.duplicated map { sn =>
      NewNode(sn, replacing=Some(this))
    })
    lazy val children = t.toSugarNode map (_.children)
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
    lazy val duplicated = s.duplicated map { s_ => NewNode(s_, replacing=Some(this)) }
    lazy val children = s.children.pure[M]
  }

  sealed trait LocalNode {
    def manifest(children: Seq[RNode]): SugarNode
  }
  object LocalNode {
    case class IntLiteral(n: Int) extends LocalNode {
      def manifest(children: Seq[RNode]) = children match {
        case Seq() => self.IntLiteral(n)
      }
    }
    case class ApicalOperator(op: SugarOperator) extends LocalNode {
      def manifest(children: Seq[RNode]) = self.ApicalOperator(op, children)
    }
    case class Pure(of: Idiom) extends LocalNode {
      def manifest(children: Seq[RNode]) = children match {
        case Seq(is) => self.Pure(of, is)
      }
    }
    case class Mystery(name: String) extends LocalNode {
      def manifest(children: Seq[RNode]) = children match {
        case Seq() => self.Mystery(name)
      }
    }
    case class NumberEditor(name: String, progress: String) extends LocalNode {
      def manifest(children: Seq[RNode]) = children match {
        case Seq() => self.NumberEditor(name, progress)
      }
    }
    case class AntiPureNameEditor(idiomKind: KindOfIdiom, progress: String) extends LocalNode {
      def manifest(children: Seq[RNode]) = children match {
        case Seq(is) => self.AntiPureNameEditor(idiomKind, progress, is)
      }
    }
    case class PureNameEditor(idiomKind: KindOfIdiom, progress: String) extends LocalNode {
      def manifest(children: Seq[RNode]) = children match {
        case Seq(is) => self.PureNameEditor(idiomKind, progress, is)
      }
    }
    case class VariableEditor(name: String, progress: String) extends LocalNode {
      def manifest(children: Seq[RNode]) = children match {
        case Seq() => self.VariableEditor(name, progress)
      }
    }
    case class Variable(name: String) extends LocalNode {
      def manifest(children: Seq[RNode]) = children match {
        case Seq() => self.Variable(name)
      }
    }
  }
}
