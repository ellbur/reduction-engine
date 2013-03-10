
object Structure2 {

  trait Node {
    val typ: Node
  }

  case class App(car: Node, cdr: Node) extends Node {
    lazy val typ = App(ReturnType, car.typ)
  }
  case object ReturnType extends Node {
    val typ = To(Set, Set)
  }
  object Set extends Node {
    val typ = Set // muahahahahaha
  }
  case object IntType extends Node {
    val typ = Set
  }
  case object Plus extends Node {
    val typ = To(IntType, To(IntType, IntType))
  }
  case class To(a: Node, b: Node) extends Node {
    val typ = Set
  }
  case class IntLiteral(x: Int) extends Node {
    val typ = IntType
  }

  def reduce(n: Node): Node = n match {
    case App(a, b) => App(reduce(a), reduce(b)) match {
      case App(ReturnType, To(a, b)) => b
      case App(App(Plus, IntLiteral(a)), IntLiteral(b)) =>
        IntLiteral(a + b)
      case n => n
    }
    case n => n
  }
}

object Test2 extends scala.App {
  import Structure2._

  val expr = App(App(Plus, IntLiteral(3)), IntLiteral(5))

  println("%s: %s" format (expr, expr.typ))
  println("...: %s" format (reduce(expr.typ)))
  println("%s: %s" format (reduce(expr), reduce(expr).typ))
  println("...: %s" format(reduce(reduce(expr).typ)))
}
