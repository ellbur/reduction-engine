
object Structure {

  trait Node {
    val typ: Node
  }

  case object Set extends Node {
    val typ = Set
  }

  case class To(car: Node, cdr: Node) extends Node {
    val typ = Set
  }

  case class RetVal(funcType: Node) extends Node {
    val typ = Set
  }

  case class App(car: Node, cdr: Node) extends Node {
    val typ = RetVal(car.typ)
  }

  case class LiftedApp(car: Node, op: Node, cdr: Node) extends Node {
    val really = App(App(op, car), cdr)
    val typ = really.typ
  }

  case object IntType extends Node {
    val typ = Set
  }

  case class IntLiteral(lit: Int) extends Node {
    val typ = IntType
  }

  case object Plus extends Node {
    val typ = To(IntType, To(IntType, IntType))
  }

  case class Reduction(attempt: PartialFunction[Node, Node])

  object Reduction {

    import Misc._

    // TODO: Descending while reducing.

    lazy val strategies: List[Reduction] = List(
      Reduction {
        case LiftedApp(car, op, cdr) =>
          reduceOrFail(App(App(op, car), cdr))
      },
      Reduction {
        case App(App(Plus, IntLiteral(x)), IntLiteral(y)) =>
          IntLiteral(x + y)
      },
      Reduction {
        case RetVal(To(a, b)) => b
      }
    )

    @throws[MatchError]
    def reduceOrFail(node: Node): Node = firstSucceeding(node, strategies map (_.attempt))

    def reduce(node: Node) =
      try
        reduceOrFail(node)
      catch {
        case _: MatchError =>
          node
      }
  }

  object Test1 extends scala.App {

    import Reduction._

    val expr = LiftedApp(IntLiteral(3), Plus, IntLiteral(4))

    println("%s: %s" format(expr, expr.typ))
    println("...: %s" format reduce(expr.typ))
    println("%s: %s" format(reduce(expr), reduce(expr).typ))
    println("...: %s" format reduce(reduce(expr).typ))
  }

}
