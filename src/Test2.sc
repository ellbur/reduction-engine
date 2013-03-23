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
val expr1 = NN(App(Nil, eI, n(3)))


StandardReductions.find[Nothing](expr1)




val A = Idiom("A", NN(Mystery(0)), NN(Mystery(1)))

val B = Idiom("B", NN(Mystery(0)), NN(Mystery(1)))

/*
val expr2 = NN(App(List(A), eI, n(3)))



StandardReductions.find[Nothing](expr2)

val expr3 = NN(App(List(A), NN(Pure(A, eI)), n(3)))



StandardReductions.find[Nothing](expr3)


val expr4 = NN(App(
  List(A, B),
  NN(Pure(A, NN(Pure(B, eI)))),
  n(3)
))





StandardReductions.find[Nothing](expr4)
*/

// The non-idiomatic form of the E-J rule.
val expr6 = NN(App(
  List(),
  eE,
  NN(App(
    List(),
    eJ,
    n(3)
  ))
))

StandardReductions.find[Nothing](expr6)

// A complicated idiomatic form.
val expr5 = NN(App(
  List(B, A),
  NN(Pure(B, NN(Pure(A, eE)))),
  NN(Pure(B, NN(App(
    List(A),
    NN(Pure(A, eJ)),
    n(3)
  ))))
))





StandardReductions.find[Nothing](expr5)


"Hello!"
