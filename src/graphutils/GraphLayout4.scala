
package graphutils

object GraphLayout4 {
  def computeLayout(
       scale: Double,
       origPosition: IndexedSeq[(Double, Double)],
       adjacency: IndexedSeq[Seq[Int]],
       disturbed: IndexedSeq[Boolean]):
    IndexedSeq[(Double,Double)] =
  {
    val N = origPosition.length

    // Step 1: Topological sort!

    val topSorted = Array.fill[Int](N)(0)
    val depths = Array.fill[Int](N)(0)
    val visited = Array.fill[Boolean](N)(false)
    var topI: Int = 0
    var startI: Int = 0
    def dfsAt(k: Int): Int = {
      if (! visited(k)) {
        visited(k) = true
        val depth =
          math.max((for (j <- adjacency(k)) yield {
            dfsAt(j)
          }).sum + adjacency(k).length - 1, 0)
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

    // Step 2: Iterate over the nodes in topoligical order.
    // If it is disturbed, set it to an average parent x offset for
    // child order, and a max y + some nice number.
    // If it is not disturbed, shift its position by the average shift
    // of its parents.

    case class ParentEdge(x: Double, y: Double, xShift: Double, yShift: Double, childOffset: Double)
    val parentEdges = Array.fill[List[ParentEdge]](N)(Nil)

    val resultingPositions = Array.fill[(Double, Double)](N)((0.0, 0.0))

    for (k <- topSorted) {
      val parents = parentEdges(k)
      val (origX, origY) = origPosition(k)

      val (nx, ny) = {
        if (parents.length == 0) {
          (origX, origY)
        }
        else {
          if (disturbed(k)) {
            val xDs = parents map { p =>
              p.x + p.childOffset * scale
            }
            val yDs = parents map { p => p.y }

            (xDs.sum / xDs.length, yDs.max + scale)
          }
          else {
            val xSs = parents map (_.xShift)
            val ySs = parents map (_.yShift)

            (origX + xSs.sum/xSs.length, origY + ySs.sum/ySs.length)
          }
        }
      }

      val sx = nx - origX
      val sy = ny - origY

      val avgDepth = (adjacency(k) map (depths(_))).sum.toDouble / adjacency(k).length

      for ((j, child) <- adjacency(k).zipWithIndex) {
        val childOffset = (child - (adjacency(k).length-1)/2.0)  * (avgDepth + 1)*2.0
        val edge = ParentEdge(nx, ny, sx, sy, childOffset)

        parentEdges(j) +:= edge
      }

      resultingPositions(k) = (nx, ny)
    }

    resultingPositions.toIndexedSeq
  }

  def main(args: Array[String]) {
    println(GraphLayout4.computeLayout(
      scale = 40.0,
      origPosition = Vector((0.0, 0.0), (0.0, 40.0), (0.0, 40.0)),
      adjacency = Vector(List(1), List(2), List()),
      disturbed = Vector(false, true, true)
    ))
  }
}
