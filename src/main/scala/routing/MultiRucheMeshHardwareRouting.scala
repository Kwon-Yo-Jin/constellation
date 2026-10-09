package constellation.routing

import chisel3._
import constellation.topology.{GridNodeIdLayout, MultiRucheMesh2D, NodeIdLayout}
import HardwareNodeId._

/** Compact coordinate routing, independent of the number of mesh nodes. */
private[routing] object MultiRucheMeshHardwareRouting {
  def dimensionOrdered(topo: MultiRucheMesh2D, firstDim: Int): HardwareRouting = {
    require(firstDim == 0 || firstDim == 1)
    val layout = NodeIdLayout(topo).asInstanceOf[GridNodeIdLayout]

    def preferredNext(node: UInt, next: UInt, dest: UInt, factors: Seq[Int]): Bool = {
      val distance = Mux(dest > node, dest - node, node - dest)
      // Later enabled levels override earlier levels when their skip fits.
      val step = factors.filter(_ > 0).foldLeft(1.U(node.getWidth.W)) { (previous, factor) =>
        Mux(distance >= factor.U, factor.U(node.getWidth.W), previous)
      }
      dest =/= node && next === Mux(dest > node, node + step, node - step)
    }

    new HardwareRouting {
      def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
        flow: HardwareRoutingFlow): Bool = {
        val (nextX, nextY) = grid(next.dst, layout)
        val (nodeX, nodeY) = grid(next.src, layout)
        val (destX, destY) = grid(flow.egressNode, layout)
        val routeX = nextY === nodeY &&
          preferredNext(nodeX, nextX, destX, topo.xRucheFactors)
        val routeY = nextX === nodeX &&
          preferredNext(nodeY, nextY, destY, topo.yRucheFactors)
        if (firstDim == 0) Mux(destX =/= nodeX, routeX, routeY)
        else Mux(destY =/= nodeY, routeY, routeX)
      }
    }
  }

  def minimal(topo: MultiRucheMesh2D): HardwareRouting = {
    val layout = NodeIdLayout(topo).asInstanceOf[GridNodeIdLayout]

    def toward(node: UInt, next: UInt, dest: UInt): Bool =
      Mux(dest > node, next > node && next <= dest,
        Mux(dest < node, next < node && next >= dest, false.B))

    new HardwareRouting {
      def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
        flow: HardwareRoutingFlow): Bool = {
        val (nextX, nextY) = grid(next.dst, layout)
        val (nodeX, nodeY) = grid(next.src, layout)
        val (destX, destY) = grid(flow.egressNode, layout)
        val routeX = nextY === nodeY && toward(nodeX, nextX, destX)
        val routeY = nextX === nodeX && toward(nodeY, nextY, destY)
        routeX || routeY
      }
    }
  }
}
