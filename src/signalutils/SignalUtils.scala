
import scalaz._
import Scalaz._
import reactive._

package object signalutils {
  implicit val signalApplicative = new Applicative[Signal] {
    override def fmap[A,B](sig: Signal[A], f: A => B) = sig.map(f)
    def pure[A](x: => A): Signal[A] = EventStream.empty.hold(x)
    override def apply[A,B](f: Signal[A => B], x: Signal[A]) = (f zip x) map { case (f, x) => f(x) }
  }

  implicit val signalMonad = new Monad[Signal] {
    override def fmap[A,B](sig: Signal[A], f: A => B) = sig.map(f)
    def pure[A](x: => A): Signal[A] = EventStream.empty.hold(x)
    override def apply[A,B](f: Signal[A => B], x: Signal[A]) = (f zip x) map { case (f, x) => f(x) }
    override def bind[A, B](a: Signal[A], f: A => Signal[B]): Signal[B] = a flatMap f
  }

  def startingWith[A](s: A)(ev: EventStream[A]) = ev.hold(s)


}
