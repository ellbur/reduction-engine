
import scalaz._
import Scalaz._

object ArgTest extends App {
  val logic = new reductionengine.logic.Logic {
    type M[+X] = X
    val monad = new Monad[M] {
      def pure[A](x: => A): M[A] = x
      def bind[A,B](x: M[A], f: A => M[B]): M[B] = f(x)
    }
  }
  import logic._

  {
    val i = NewNode(OperatorLiteral(I))
    val ix = NewNode(App(i, NewNode(Mystery("x"))))

    println(ix.reductions)
  }
}
