
package graphutils

import org.apache.commons.math3.analysis.MultivariateFunction
import org.apache.commons.math3.optim.{SimpleValueChecker, InitialGuess, MaxIter, MaxEval}
import org.apache.commons.math3.optim.nonlinear.scalar.{ObjectiveFunction, GoalType}
import org.apache.commons.math3.optim.nonlinear.scalar.noderiv.{NelderMeadSimplex, SimplexOptimizer, PowellOptimizer}
import scala.math._

object GraphLayout2 {
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

    def forces(points: IndexedSeq[(Double,Double)]): IndexedSeq[(Double, Double)] = {
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
            val hookF = (dist - naturalEdgeLen) / naturalEdgeLen

            val (nx, ny) = (dx/dist, dy/dist)

            i foreach { i =>
              forcesX(i) += hookF * nx
              forcesY(i) += hookF * ny
            }
            j foreach { j =>
              forcesX(j) += - hookF * nx
              forcesY(j) += - hookF * ny
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
            val hookF = (dist - naturalSeparation) / naturalSeparation * 2.0
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

      def clean(t: Double) =
        if (t.isNaN)
          random
        else
          t

      ((forcesX map clean _) zip (forcesY map clean _)).toIndexedSeq
    }
  }

  def computeLayout(
     movableNodes: IndexedSeq[MovableNode],
     fixedNodes: IndexedSeq[FixedNode],
     naturalEdgeLen: Double,
     naturalSeparation: Double): IndexedSeq[ResultingPosition] =
  {
    val setup = new InLayoutComputations(movableNodes, fixedNodes, naturalEdgeLen, naturalSeparation)

    val N = movableNodes.length
    val vX: Array[Double] = Array.fill(N)(0.0)
    val vY: Array[Double] = Array.fill(N)(0.0)

    val (startX, startY) = (movableNodes map {
      case MovableNode(x, y, _) => (x, y)
    }).unzip

    val x: Array[Double] = startX.toArray
    val y: Array[Double] = startY.toArray

    val maxIter = 30
    val dt = sqrt(2*naturalEdgeLen/1.0) / maxIter * 4.0
    val damping = pow(0.01, 1.0/maxIter.toDouble)

    //println(s"maxIter = $maxIter")
    //println(s"dt = $dt")
    //println(s"damping = $damping")

    for (iter <- 1 to maxIter) {
      val points = (x zip y).toIndexedSeq
      val forces = setup.forces(points)

      for (i <- 0 to N-1) {
        vX(i) += forces(i)._1 * dt
        vY(i) += forces(i)._2 * dt

        x(i) += vX(i) * dt
        y(i) += vY(i) * dt

        vX(i) *= damping
        vY(i) *= damping
      }

      //println((x zip y).toList)
    }

    ((x zip y) map {
      case (x, y) => ResultingPosition(x, y)
    }).toIndexedSeq
  }

  def main(args: Array[String]) {
    computeLayout(
      movableNodes = Vector(
        MovableNode(+1000.0, 1000.0, Seq()),
        MovableNode(-1000.0, 1000.0, Seq())
      ),
      fixedNodes = Vector(
        FixedNode(0.0, 0.0, Seq(
          MovableChild(0),
          MovableChild(1)
        ))
      ),
      naturalEdgeLen = 1000.0,
      naturalSeparation = 1000.0
    )
  }
}
