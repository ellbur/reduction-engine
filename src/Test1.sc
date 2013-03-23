
/**
 * Created with IntelliJ IDEA.
 * User: owen
 * Date: 3/22/13
 * Time: 10:21 PM
 * To change this template use File | Settings | File Templates.
 */

import reductionengine.sugar._
import reductionengine.sugar.{ NewNode => NN, AlreadyThere => AT }
import reductionengine.{logic => l}

val f = NN(Open("f"))

val x = NN(Open("x"))
val y = NN(Open("y"))

val app = NN(Open("app"))
val pure = NN(Open("pure"))

f(x)(y)
val t1 = app(f)
t1(x)(y)

app(app(f)(x))(y)
val S = NN(Open("S"))
val S2 = NN(Open("S₂"))
val K_ = NN(Open("K_"))
val K_x = NN(Open("K_x"))
val Kx_ = NN(Open("Kx_"))
val K__ = NN(Open("K__"))
val t1_x = app(f)
val t2_xy = S(K_(app))(t1_x)
val t3_xyz = S2(K__(app))(t2_xy)


val app_ = S2(Kx_(app))(K_x(pure))
val t2_x_ = S(K_(app_))(t1_x)


