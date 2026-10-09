package constellation.routing

import constellation.topology.{
  PhysicalTopology, Mesh2DLikePhysicalTopology,
  UnidirectionalRucheTorus2D, BidirectionalRucheTorus2D}

private object RucheTorus2DRouting {
  def forwardDistance(from: Int, to: Int, size: Int): Int =
    (to - from + size) % size

  // Antipodal links and equal-distance routes use the positive direction.
  def direction(from: Int, to: Int, size: Int, bidirectional: Boolean): Int = {
    val forward = forwardDistance(from, to, size)
    if (bidirectional && forward > size - forward) -1 else 1
  }

  def preferredNext(node: Int, next: Int, dest: Int, size: Int,
    factor: Int, bidirectional: Boolean): Boolean = {
    val forward = forwardDistance(node, dest, size)
    val distance = if (bidirectional) forward min (size - forward) else forward
    // In a bidirectional torus, factors r and size-r connect the same neighbors.
    val skip = if (bidirectional) factor min (size - factor) else factor
    val step = if (skip > 0 && distance >= skip) skip else 1
    distance > 0 && next == (node + direction(node, dest, size, bidirectional) * step + size) % size
  }
}

/** Dateline VC rules shared by the two Ruche torus variants.
  *
  * VC 0 is used on and after the wraparound hop; nonzero VCs are used before it.
  * Detect crossing by coordinates, since a skip can cross without visiting an
  * endpoint of the dimension. Injection and a dimension turn start a fresh phase.
  * This relation alone does not constrain dimension order or destination progress.
  */
private class RucheTorus2DDatelineRelation(
  topo: Mesh2DLikePhysicalTopology,
  xRucheFactor: Int,
  yRucheFactor: Int,
  bidirectional: Boolean
) extends RoutingRelation(topo) {
  import RucheTorus2DRouting._

  private def dimension(c: ChannelRoutingInfo): Int =
    if (c.src / topo.nX == c.dst / topo.nX) 0 else 1

  private def coordinate(node: Int, dim: Int): Int =
    if (dim == 0) node % topo.nX else node / topo.nX

  def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
    if (nxtC.isEgress) return nxtC.src == flow.egressNode
    require(nxtC.n_vc >= 2 && (srcC.isIngress || srcC.n_vc >= 2),
      "Ruche torus dateline routing requires at least two virtual channels per network link")
    if (nxtC.src == flow.egressNode || !topo.topo(nxtC.src, nxtC.dst)) return false

    val dim = dimension(nxtC)
    val size = if (dim == 0) topo.nX else topo.nY
    val node = coordinate(nxtC.src, dim)
    val next = coordinate(nxtC.dst, dim)
    val dir = direction(node, next, size, bidirectional)
    val crossing = if (dir > 0) next < node else next > node
    val start = srcC.isIngress || dimension(srcC) != dim

    if (start) {
      if (crossing) nxtC.vc == 0 else nxtC.vc != 0
    } else if (direction(coordinate(srcC.src, dim), node, size, bidirectional) != dir) {
      false
    } else if (crossing) {
      srcC.vc != 0 && nxtC.vc == 0
    } else if (srcC.vc == 0) {
      nxtC.vc == 0
    } else {
      nxtC.vc != 0 && nxtC.vc <= srcC.vc
    }
  }

  override def getNPrios(c: ChannelRoutingInfo): Int = 2
  override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int = {
    if (nxtC.isIngress || nxtC.isEgress) return 0
    val dim = dimension(nxtC)
    val size = if (dim == 0) topo.nX else topo.nY
    val factor = if (dim == 0) xRucheFactor else yRucheFactor
    val delta = forwardDistance(coordinate(nxtC.src, dim), coordinate(nxtC.dst, dim), size)
    val ruche = factor != 0 && (delta == factor || (bidirectional && delta == size - factor))
    if (ruche) 0 else 1
  }
}

/** Unidirectional Ruche dateline constraints and Ruche-first allocation priority.
  * As with the ordinary torus base relation, add dimension ordering for deadlock freedom.
  */
object UnidirectionalRucheTorus2DDatelineRouting {
  def apply(): PhysicalTopology => RoutingRelation = (topo: PhysicalTopology) => topo match {
    case topo: UnidirectionalRucheTorus2D =>
      new RucheTorus2DDatelineRelation(topo, topo.xRucheFactor, topo.yRucheFactor, bidirectional = false)
  }
}

/** Bidirectional Ruche dateline constraints and Ruche-first allocation priority.
  * Direction changes within a dimension are disallowed; dimension turns remain unrestricted.
  */
object BidirectionalRucheTorus2DDatelineRouting {
  def apply(): PhysicalTopology => RoutingRelation = (topo: PhysicalTopology) => topo match {
    case topo: BidirectionalRucheTorus2D =>
      new RucheTorus2DDatelineRelation(topo, topo.xRucheFactor, topo.yRucheFactor, bidirectional = true)
  }
}

/** X-then-Y Ruche routing with two dateline VCs per dimension, reused at the turn.
  * A fitting Ruche skip is selected before a regular hop. Each dimension advances
  * in one direction and crosses its dateline at most once. Dependencies decrease
  * VC phase at crossing and only turn X -> Y, so the channel dependency graph is acyclic.
  */
object DimensionOrderedUnidirectionalRucheTorus2DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: UnidirectionalRucheTorus2D => new RoutingRelation(topo) {
      val base = UnidirectionalRucheTorus2DDatelineRouting()(topo)
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
        val (nextX, nextY) = (nxtC.dst % topo.nX, nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX, nxtC.src / topo.nX)
        val (destX, destY) = (flow.egressNode % topo.nX, flow.egressNode / topo.nX)
        val sel = if (destX != nodeX) {
          nextY == nodeY && RucheTorus2DRouting.preferredNext(
            nodeX, nextX, destX, topo.nX, topo.xRucheFactor, bidirectional = false)
        } else {
          nextX == nodeX && RucheTorus2DRouting.preferredNext(
            nodeY, nextY, destY, topo.nY, topo.yRucheFactor, bidirectional = false)
        }
        sel && base(srcC, nxtC, flow)
      }
      override def getNPrios(c: ChannelRoutingInfo): Int = base.getNPrios(c)
      override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int =
        base.getPrio(srcC, nxtC, flow)
    }
  }
}

/** X-then-Y routing using the shorter direction in each torus dimension.
  * Positive direction wins a distance tie; Ruche skips are preferred to regular hops.
  * The same dateline argument as the unidirectional policy gives deadlock freedom.
  * An antipodal skip finishes its dimension, so it cannot join opposite-direction cycles.
  */
object DimensionOrderedBidirectionalRucheTorus2DDatelineRouting {
  def apply() = (topo: PhysicalTopology) => topo match {
    case topo: BidirectionalRucheTorus2D => new RoutingRelation(topo) {
      val base = BidirectionalRucheTorus2DDatelineRouting()(topo)
      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
        val (nextX, nextY) = (nxtC.dst % topo.nX, nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX, nxtC.src / topo.nX)
        val (destX, destY) = (flow.egressNode % topo.nX, flow.egressNode / topo.nX)
        val sel = if (destX != nodeX) {
          nextY == nodeY && RucheTorus2DRouting.preferredNext(
            nodeX, nextX, destX, topo.nX, topo.xRucheFactor, bidirectional = true)
        } else {
          nextX == nodeX && RucheTorus2DRouting.preferredNext(
            nodeY, nextY, destY, topo.nY, topo.yRucheFactor, bidirectional = true)
        }
        sel && base(srcC, nxtC, flow)
      }
      override def getNPrios(c: ChannelRoutingInfo): Int = base.getNPrios(c)
      override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Int =
        base.getPrio(srcC, nxtC, flow)
    }
  }
}
