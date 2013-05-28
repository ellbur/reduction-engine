
package graphutils

object GraphLayout4 {
  case class Node(origPosition: Option[(Double, Double)], disturb: Boolean, adjacency: Seq[Int])

  def computeLayout(
       hScale: Double,
       vScale: Double,
       center: (Double, Double),
       initial: IndexedSeq[Node]):
    IndexedSeq[(Double,Double)] =
  {
    val N = initial.length

    // Step 1: Topological sort!

    val topSorted = Array.fill[Int](N)(0)
    val depths = Array.fill[Int](N)(0)
    val visited = Array.fill[Boolean](N)(false)
    var topI: Int = 0
    var startI: Int = 0
    def dfsAt(k: Int): Int = {
      if (! visited(k)) {
        visited(k) = true
        val children = initial(k).adjacency
        val depth =
          math.max((for (j <- children) yield {
            dfsAt(j)
          }).sum + children.length - 1, 0)
        topSorted(N - topI - 1) = k
        topI += 1
        depths(k) = depth
        depth
      }
      else
        depths(k)
    }
    while (topI < N) {
      while (visited(startI))
        startI += 1
      dfsAt(startI)
    }

    // Step 2: Iterate over the nodes in topological order.
    // If it is disturbed, set it to an average parent x offset for
    // child order, and a max y + some nice number.
    // If it is not disturbed, shift its position by the average shift
    // of its parents.

    case class ParentEdge(x: Double, y: Double, xShift: Double, yShift: Double, childOffset: Double)
    val parentEdges = Array.fill[List[ParentEdge]](N)(Nil)

    val resultingPositions = Array.fill[(Double, Double)](N)((0.0, 0.0))

    for (k <- topSorted) {
      val initially = initial(k)
      val parents = parentEdges(k)

      val (nx, ny) = {
        if (parents.length == 0) {
          initially.origPosition match {
            case Some((origX, origY)) =>
              (origX, origY)
            case None =>
              center
          }
        }
        else {
          def totallyNew = {
            val xDs = parents map { p =>
              p.x + p.childOffset * hScale
            }
            val yDs = parents map { p => p.y }

            (xDs.sum / xDs.length, yDs.max + vScale*parents.length)
          }

          initially.origPosition match {
            case None => totallyNew
            case Some(_) if initially.disturb => totallyNew
            case Some((origX, origY)) =>
              val xSs = parents map (_.xShift)
              val ySs = parents map (_.yShift)

              (origX + xSs.sum/xSs.length, origY + ySs.sum/ySs.length)
          }
        }
      }

      val (sx, sy) = initially.origPosition match {
        case Some((origX, origY)) =>
          (nx - origX, ny - origY)
        case None =>
          (0.0, 0.0)
      }

      val adjacency = initially.adjacency

      val avgDepth = (adjacency map (depths(_))).sum.toDouble / adjacency.length

      for ((j, child) <- adjacency.zipWithIndex) {
        val childOffset = (child - (adjacency.length-1)/2.0)  * (avgDepth + 1)*2.0
        val edge = ParentEdge(nx, ny, sx, sy, childOffset)

        parentEdges(j) +:= edge
      }

      resultingPositions(k) = (nx, ny)
    }

    resultingPositions.toIndexedSeq
  }
}
