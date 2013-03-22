
package graphutils

import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.optim.{SimpleValueChecker, InitialGuess, MaxIter, MaxEval}
import org.apache.commons.math3.optim.nonlinear.scalar.{ObjectiveFunction, GoalType}
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.{NelderMeadSimplex, SimplexOptimizer, PowellOptimizer}

object GraphLayout {
  case class MovableNode(startX: Double, startY: Double, children: Seq[Child])
  case class FixedNode(x: Double, y: Double, children: Seq[Child])

  sealed trait Child
  case class MovableChild(n: Int) extends Child
  case class FixedChild(n: Int) extends Child

  case class ResultingPosition(x: Double, y: Double)

  class InLayoutComputations(movableNodes: IndexedSeq[MovableNode],
                             fixedNodes: IndexedSeq[FixedNode],
                             naturalEdgeLen: Double,
                             naturalSeparation: Double)
  {
    import scala.math._

    def cost(params: IndexedSeq[Double]): Double = {
      val points = (params grouped 2 map { case Seq(x, y) => (x, y) }).toIndexedSeq

      def lookup(c: Child): (Double, Double) = c match {
        case MovableChild(n) => points(n)
        case FixedChild(n) =>
          val node = fixedNodes(n)
          (node.x, node.y)
      }

      val current =
        {
          (
            movableNodes zip points map {
              case (n, p) => (n.children, p)
            }
          ) ++ (
            fixedNodes map {
              case FixedNode(x, y, ch) => (ch, (x, y))
            }
          )
        }.toSeq

      val edgeRelatedTotal: Double =
        (
          current map { case (children, (x, y)) =>
            if (children.length == 0) {
              0.0
            }
            else {
              val childPoints = children map lookup _

              val angularSeparation = Pi / 3.0 / children.length.toDouble

              val edgeTotal: Double =
                (
                  childPoints map {
                    case (cx, cy) =>
                      val dist = sqrt((cx-x)*(cx-x) + (cy-y)*(cy-y))
                      val fromEdge = abs(dist - naturalEdgeLen) / naturalEdgeLen

                      val fromGravity = (y - cy) / naturalEdgeLen / 5.0

                      fromEdge + fromGravity
                  }
                ).sum

              val angleTotal: Double =
              {
                val childAngles = childPoints map {
                  case (cx, cy) =>
                    Math.atan2(cy - y, cx - x)
                }
                val angleDiffs = ((Seq(0.0) ++ childAngles ++ Seq(Pi)) sliding 2 map {
                  case Seq(th1, th2) => th2 - th1
                }).toList

                (
                  angleDiffs map { diff =>
                    if (diff < angularSeparation) {
                      val b = (angularSeparation - diff) / angularSeparation
                      b
                    }
                    else
                      0.0
                  }
                ).sum
              }

              val imbalanceTotal: Double =
              {
                val childHeights: Seq[Double] = childPoints map (_._2)
                val meanHeight = childHeights.sum / childHeights.length

                (childHeights map (h => abs(h - meanHeight)/naturalEdgeLen)).sum
              }

              edgeTotal + angleTotal + imbalanceTotal
            }
          }
        ).sum

      val separationTotal: Double = {
        for (Seq((_, (x1, y1)), (_, (x2, y2))) <- current.combinations(2)) yield {

          val dist = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))
          if (dist < naturalSeparation) {
            val b = (naturalSeparation - dist) / naturalSeparation
            b
          }
          else
            0.0
        }
      }.sum

      edgeRelatedTotal + separationTotal
    }
  }

  def computeLayout(
     movableNodes: IndexedSeq[MovableNode],
     fixedNodes: IndexedSeq[FixedNode],
     naturalEdgeLen: Double,
     naturalSeparation: Double): IndexedSeq[ResultingPosition] =
  {
    val setup = new InLayoutComputations(movableNodes, fixedNodes, naturalEdgeLen, naturalSeparation)
    val guess: Array[Double] = (movableNodes flatMap { n => Seq(n.startX, n.startY) }).toArray

    val result = (new SimplexOptimizer(new SimpleValueChecker(1e-5, 1e-5, 500))).optimize(
      GoalType.MINIMIZE,
      // https://issues.apache.org/jira/browse/MATH-902
      new MaxEval(10000),
      new NelderMeadSimplex(guess.length),
      new InitialGuess(guess),
      new ObjectiveFunction(new MultivariateFunction {
        def value(params: Array[Double]): Double = setup.cost(params)
      })
    )

    val endParams = result.getPoint

    (
      endParams.toIndexedSeq grouped 2 map {
        case Seq(x, y) => ResultingPosition(x, y)
      }
    ).toIndexedSeq
  }

  def main(args: Array[String]) {
    println(computeLayout(
      movableNodes = Vector(
        MovableNode(+1.0, 1.0, Seq()),
        MovableNode(-1.0, 1.0, Seq())
      ),
      fixedNodes = Vector(
        FixedNode(0.0, 0.0, Seq(
          MovableChild(0),
          MovableChild(1)
        ))
      ),
      naturalEdgeLen = 1.0,
      naturalSeparation = 1.0
    ))
  }
}
