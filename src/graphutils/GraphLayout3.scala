
package graphutils

import org.apache.commons.math3.analysis.{MultivariateVectorFunction, MultivariateFunction}
import org.apache.commons.math3.optim.{SimpleValueChecker, InitialGuess, MaxIter, MaxEval}
import org.apache.commons.math3.optim.nonlinear.scalar.{ObjectiveFunctionGradient, ObjectiveFunction, GoalType}
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.{NelderMeadSimplex, SimplexOptimizer, PowellOptimizer}
import scala.math._
import org.apache.commons.math3.optim.nonlinear.scalar.gradient.NonLinearConjugateGradientOptimizer

object GraphLayout3 {
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

    def negativeForces(points: IndexedSeq[(Double,Double)]): IndexedSeq[(Double,Double)] = {
      def lookup(c: Child): (Option[Int], (Double, Double)) = c match {
        case MovableChild(n) => (Some(n), points(n))
        case FixedChild(n) =>
          val node = fixedNodes(n)
          (None, (node.x, node.y))
      }

      val forcesX: Array[Double] = Array.fill(points.length)(0.0)
      val forcesY: Array[Double] = Array.fill(points.length)(0.0)

      val allNodes =
        {
          (
            movableNodes.zipWithIndex zip points map {
              case ((n, i), p) => (Some(i), n.children, p)
            }
          ) ++ (
            fixedNodes map {
              case FixedNode(x, y, ch) => (None, ch, (x, y))
            }
          )
        }.toSeq

      // Edge-related forces.
      for ((i, children, (x, y)) <- allNodes) {
        for ((j, (cx, cy)) <- children map lookup _) {
          if (!i.isEmpty || !j.isEmpty) {
            val (dx, dy) = (cx - x, cy - y)

            // Edge spring forces.
            val dist = sqrt(dx*dx + dy*dy)
            val hookK = -1.0 / naturalEdgeLen
            val hookDX = dist - naturalEdgeLen
            val hookF = hookDX * hookK

            val (nx, ny) = (dx/dist, dy/dist)

            i foreach { i =>
              forcesX(i) += - hookF * nx
              forcesY(i) += - hookF * ny
            }
            j foreach { j =>
              forcesX(j) += hookF * nx
              forcesY(j) += hookF * ny
            }

            // Gravity.
            i foreach { i =>
              forcesY(i) += -0.2
            }
            j foreach { j =>
              forcesY(j) += +0.2
            }
          }
        }
      }

      // Pair-related forces.
      for (Seq(nodeA, nodeB) <- allNodes.combinations(2)) {
        val (i, _, (x1, y1)) = nodeA
        val (j, _, (x2, y2)) = nodeB

        if (!i.isEmpty || !j.isEmpty) {
          val (dx, dy) = (x2 - x1, y2 - y1)
          val dist = sqrt(dx*dx + dy*dy)

          if (dist < naturalSeparation) {
            val hookDX = dist - naturalSeparation
            val hookK = 1.0 / naturalSeparation * 2.0
            val hookF = hookK * hookDX
            val (nx, ny) = (dx/dist, dy/dist)

            i foreach { i =>
              forcesX(i) += hookF * nx
              forcesY(i) += hookF * ny
            }
            j foreach { j =>
              forcesX(j) += - hookF * nx
              forcesY(j) += - hookF * ny
            }
          }
        }
      }

      def cleanMinus(t: Double) =
        if (t.isNaN)
          random - random
        else
          - t

      ((forcesX map cleanMinus _) zip (forcesY map cleanMinus _)).toIndexedSeq
    }

    def energy(points: IndexedSeq[(Double,Double)]): Double = {
      def lookup(c: Child): (Option[Int], (Double, Double)) = c match {
        case MovableChild(n) => (Some(n), points(n))
        case FixedChild(n) =>
          val node = fixedNodes(n)
          (None, (node.x, node.y))
      }

      var energy: Double = 0.0

      val allNodes =
        {
          (
            movableNodes.zipWithIndex zip points map {
              case ((n, i), p) => (Some(i), n.children, p)
            }
          ) ++ (
            fixedNodes map {
              case FixedNode(x, y, ch) => (None, ch, (x, y))
            }
          )
        }.toSeq

      // Edge-related forces.
      for ((i, children, (x, y)) <- allNodes) {
        for ((j, (cx, cy)) <- children map lookup _) {
          if (!i.isEmpty || !j.isEmpty) {
            val (dx, dy) = (cx - x, cy - y)

            // Edge spring forces.
            val dist = sqrt(dx*dx + dy*dy)
            val hookK = -1.0 / naturalEdgeLen
            val hookDX = dist - naturalEdgeLen

            energy += 0.5*hookK*hookDX*hookDX

            // Gravity.
            energy += -dy*0.2
          }
        }
      }

      // Pair-related forces.
      for (Seq(nodeA, nodeB) <- allNodes.combinations(2)) {
        val (i, _, (x1, y1)) = nodeA
        val (j, _, (x2, y2)) = nodeB

        if (!i.isEmpty || !j.isEmpty) {
          val (dx, dy) = (x2 - x1, y2 - y1)
          val dist = sqrt(dx*dx + dy*dy)

          if (dist < naturalSeparation) {
            val hookDX = dist - naturalSeparation
            val hookK = 1.0 / naturalSeparation * 2.0
            val hookF = hookK * hookDX

            energy += 0.5*hookK*hookDX*hookDX
          }
        }
      }

      if (energy.isNaN)
        Double.MaxValue
      else
        energy
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

    def decode(params: Array[Double]): IndexedSeq[(Double, Double)] =
      (params.toSeq grouped 2 map {
        case Seq(x, y) => (x, y)
      }).toIndexedSeq

    def encode(x: Traversable[(Double, Double)]): Array[Double] =
      (x map { case (a, b) => Seq(a, b) }).flatten.toArray

    val formula = NonLinearConjugateGradientOptimizer.Formula.POLAK_RIBIERE
    val optimizer = new NonLinearConjugateGradientOptimizer(formula, new SimpleValueChecker(1e-5, 1e-5, movableNodes.length*2 + 20))
    val result = optimizer.optimize(
      GoalType.MINIMIZE,
      // https://issues.apache.org/jira/browse/MATH-902
      new MaxEval(10000),
      new InitialGuess(guess),
      new ObjectiveFunction(new MultivariateFunction {
        def value(params: Array[Double]): Double = setup.energy(decode(params))
      }),
      new ObjectiveFunctionGradient(new MultivariateVectorFunction {
        def value(params: Array[Double]): Array[Double] = encode(setup.negativeForces(decode(params)))
      })
    )
    val endParams = result.getPoint
    println(optimizer.getEvaluations)

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
