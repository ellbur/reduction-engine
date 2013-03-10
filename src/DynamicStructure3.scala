
import scala.language.implicitConversions

object DynamicStructure3 extends scala.App {

  trait Node[+B]

  case class App[+B](car: B, cdr: B) extends Node[B]
  object AppYo {
    def unapply[B<:NodeLike[B]](b: B): Option[(B, B)] = b.toNode match {
      case App(car, cdr) => Some((car, cdr))
      case _ => None
    }
  }

  case class IntLiteral(n: Int) extends Node[Nothing]

  object IntLiteralYo {
    def unapply(b: ANodeLike): Option[Int] = b.toNode match {
      case IntLiteral(n) => Some(n)
      case _ => None
    }
  }

  object Magic23 {
    def unapply[B<:NodeLike[B]](b: B): Boolean = b match {
      case AppYo(IntLiteralYo(2), IntLiteralYo(3)) => true
      case _ => false
    }
  }

  trait NodeLike[B <: NodeLike[B]] {
    def toNode: Node[B]
  }

  trait ANodeLike {
    type B <: NodeLike[B]
    val n: B
  }
  implicit def makeANodeLike[S<:NodeLike[S]](b: S) = new ANodeLike {
    type B = S
    val n = b
  }
  implicit def unmakeANodeLike(anl: ANodeLike): anl.B = anl.n

  trait DefaultNodeLike[-Of] {
    type B <: NodeLike[B]
    def promote(b: Of): B
  }
  implicit def makeANodeLike2[S](b: S)(implicit d: DefaultNodeLike[S]) = new ANodeLike {
    type B = d.B
    val n = d.promote(b)
  }

  object IntLiteralYo2 {
    def unapply(b: ANodeLike): Option[Int] = b.toNode match {
      case x @ IntLiteral(n) => Some(n)
      case other => None
    }
  }

  trait BasicBubble extends NodeLike[BasicBubble]
  implicit object BasicBubbleDefaults extends DefaultNodeLike[BasicBubble] {
    type B = BasicBubble
    def promote(b: BasicBubble) = b
  }

  case class BasicApp(car: BasicBubble, cdr: BasicBubble) extends BasicBubble {
    def toNode = App(car, cdr)
  }

  case class BasicIntLiteral(n: Int) extends BasicBubble {
    def toNode = IntLiteral(n)
  }

  val f1: BasicBubble = BasicApp(BasicIntLiteral(2), BasicIntLiteral(3))
  val f2: BasicBubble = BasicApp(BasicIntLiteral(2), BasicIntLiteral(4))

  println(Magic23.unapply(f1))
  println(Magic23.unapply(f2))

  //IntLiteralYo2.unapply(BasicIntLiteral(5): BasicBubble)

  (BasicIntLiteral(5): ANodeLike) match {
    case IntLiteralYo2(yo) => println(yo)
    case _ => println(":(")
  }
}
