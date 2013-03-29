/**
 * Created with IntelliJ IDEA.
 * User: owen
 * Date: 3/23/13
 * Time: 12:49 PM
 * To change this template use File | Settings | File Templates.
 */

import reductionengine.logic._
import reductionengine.logic.{NewNode => NN}
import NodeLike._

val eI = NN(OperatorLiteral(I))
val eJ = NN(OperatorLiteral(J))
val eE = NN(OperatorLiteral(E))

def n(x: Int) = NN(IntLiteral(x))
val expr1 = NN(App(eI, n(3)))


StandardReductions.find[Nothing](expr1)





val A = Idiom("A", NN(Mystery(0)), NN(Mystery(1)))

val B = Idiom("B", NN(Mystery(0)), NN(Mystery(1)))

// Used to be:
// val expr2 = NN(App(List(A), eI, n(3)))
val expr2 = NN(Pure(A,
  NN(App(
    NN(AntiPure(A, eI)),
    NN(AntiPure(A, n(3)))
  ))
))


StandardReductions.find[Nothing](expr2)

// Used to be:
// val expr3 = NN(App(List(A), NN(Pure(A, eI)), n(3)))
val expr3 = NN(Pure(A,
  NN(App(
    eI,
    NN(AntiPure(A, n(3)))
  ))
))


StandardReductions.find[Nothing](expr3)

val expr4 = NN(Pure(A, NN(Pure(B,
 NN(App(
  eI,
  NN(AntiPure(B, NN(AntiPure(A,
    n(3)
  ))))
 ))
))))


StandardReductions.find[Nothing](expr4)


// The non-idiomatic form of the E-J rule.
val expr6 = NN(App(
  eE,
  NN(App(
    eJ,
    n(3)
  ))
))


StandardReductions.find[Nothing](expr6)


// A complicated idiomatic form.
val expr5 = NN(Pure(A, NN(Pure(B,
  NN(App(
    eE,
    NN(App(
      eJ,
      NN(AntiPure(A, n(3)))
    ))
  ))
))))


StandardReductions.find[Nothing](expr5)


val expr7 = NN(Pure(A,
  NN(App(
    eI,
    NN(AntiPure(A, n(3)))
  ))
))


StandardReductions.find[Nothing](expr7)


"Hello!"
