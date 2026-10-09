package constellation.routing

import constellation.topology.{MultiRucheMesh2D, PhysicalTopology}

private object MultiRucheMeshRouting {
  def validate(topo: MultiRucheMesh2D): Unit = {
    // Zero disables an axis at that level; compare only its enabled factors.
    val xFactors = topo.xRucheFactors.filter(_ != 0)
    val yFactors = topo.yRucheFactors.filter(_ != 0)
    require(xFactors.zip(xFactors.drop(1)).forall { case (a, b) => a < b },
      s"MultiRucheMesh2D routing requires strictly increasing enabled X Ruche factors: ${topo.xRucheFactors}")
    require(yFactors.zip(yFactors.drop(1)).forall { case (a, b) => a < b },
      s"MultiRucheMesh2D routing requires strictly increasing enabled Y Ruche factors: ${topo.yRucheFactors}")
  }

  def priority(topo: MultiRucheMesh2D, c: ChannelRoutingInfo): Int = {
    if (c.isIngress || c.isEgress) return 0
    val (srcX, srcY) = (c.src % topo.nX, c.src / topo.nX)
    val (dstX, dstY) = (c.dst % topo.nX, c.dst / topo.nX)
    val level = if (srcY == dstY) {
      topo.xRucheFactors.lastIndexWhere(f => f != 0 && (srcX - dstX).abs == f)
    } else if (srcX == dstX) {
      topo.yRucheFactors.lastIndexWhere(f => f != 0 && (srcY - dstY).abs == f)
    } else {
      -1
    }
    // Smaller priority values win. Parallel factor-one and local links have
    // identical channel routing metadata and therefore share a priority.
    if (level >= 0) topo.nLevels - 1 - level else topo.nLevels
  }
}

private abstract class MultiRucheMeshRoutingRelation(topo: MultiRucheMesh2D)
    extends RoutingRelation(topo) {
  MultiRucheMeshRouting.validate(topo)

  override def getNPrios(c: ChannelRoutingInfo): Int = topo.nLevels + 1
  override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo,
    flow: FlowRoutingInfo): Int = MultiRucheMeshRouting.priority(topo, nxtC)
}

/** Dimension-ordered routing taking the highest-level fitting Ruche skip.
  * The regular mesh handles any remainder. Dependencies advance monotonically
  * within a dimension and turn only in firstDim order, so one VC is sufficient
  * for deadlock freedom. Skip selection does not depend on the VC allocator.
  */
object MultiRucheMesh2DDimensionOrderedRouting {
  def apply(firstDim: Int = 0): PhysicalTopology => RoutingRelation =
    (topo: PhysicalTopology) => topo match {
      case topo: MultiRucheMesh2D => new MultiRucheMeshRoutingRelation(topo) {
        require(firstDim == 0 || firstDim == 1)

        private def preferredNext(node: Int, next: Int, dest: Int, factors: Seq[Int]): Boolean = {
          val distance = (dest - node).abs
          val step = factors.reverseIterator.find(f => f != 0 && f <= distance).getOrElse(1)
          node != dest && next == node + (if (dest > node) step else -step)
        }

        def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
          val (nextX, nextY) = (nxtC.dst % topo.nX, nxtC.dst / topo.nX)
          val (nodeX, nodeY) = (nxtC.src % topo.nX, nxtC.src / topo.nX)
          val (destX, destY) = (flow.egressNode % topo.nX, flow.egressNode / topo.nX)
          val routeX = nextY == nodeY && preferredNext(nodeX, nextX, destX, topo.xRucheFactors)
          val routeY = nextX == nodeX && preferredNext(nodeY, nextY, destY, topo.yRucheFactors)

          if (firstDim == 0) {
            if (destX != nodeX) routeX else routeY
          } else {
            if (destY != nodeY) routeY else routeX
          }
        }

        override val hardwareRouting = Some(MultiRucheMeshHardwareRouting.dimensionOrdered(topo, firstDim))
      }
    }
}

/** Coordinate-minimal adaptive routing, not deadlock-free on its own.
  * All local and Ruche hops toward the destination without overshooting remain
  * legal, including a Ruche hop after a regular hop. A prioritizing allocator
  * selects higher Ruche levels first; this is not a minimum-hop guarantee.
  */
object MultiRucheMesh2DMinimalRouting {
  def apply(): PhysicalTopology => RoutingRelation = (topo: PhysicalTopology) => topo match {
    case topo: MultiRucheMesh2D => new MultiRucheMeshRoutingRelation(topo) {
      private def toward(node: Int, next: Int, dest: Int): Boolean =
        if (dest > node) next > node && next <= dest
        else if (dest < node) next < node && next >= dest
        else false

      def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean = {
        val (nextX, nextY) = (nxtC.dst % topo.nX, nxtC.dst / topo.nX)
        val (nodeX, nodeY) = (nxtC.src % topo.nX, nxtC.src / topo.nX)
        val (destX, destY) = (flow.egressNode % topo.nX, flow.egressNode / topo.nX)
        val routeX = nextY == nodeY && toward(nodeX, nextX, destX)
        val routeY = nextX == nodeX && toward(nodeY, nextY, destY)
        routeX || routeY
      }

      override val hardwareRouting = Some(MultiRucheMeshHardwareRouting.minimal(topo))
    }
  }
}

/** Adaptive minimal routing with dimension-ordered escape VCs.
  * Escape VCs are [0, nEscapeChannels); a packet cannot return to adaptive VCs
  * after entering escape. Higher Ruche levels take priority across both VC
  * classes, with adaptive preferred over escape at the same level. Configure
  * at least one additional VC to enable the adaptive part of this policy.
  */
object MultiRucheMesh2DEscapeRouting {
  def apply(firstDim: Int = 0, nEscapeChannels: Int = 1): PhysicalTopology => RoutingRelation =
    (topo: PhysicalTopology) => topo match {
      case topo: MultiRucheMesh2D => new RoutingRelation(topo) {
        require(nEscapeChannels > 0, "MultiRucheMesh2D escape routing requires at least one escape VC")
        private val base = EscapeChannelRouting(
          escapeRouter = MultiRucheMesh2DDimensionOrderedRouting(firstDim),
          normalRouter = MultiRucheMesh2DMinimalRouting(),
          nEscapeChannels = nEscapeChannels)(topo)

        def rel(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo, flow: FlowRoutingInfo): Boolean =
          base(srcC, nxtC, flow)
        override def isEscape(c: ChannelRoutingInfo, vNetId: Int): Boolean = base.isEscape(c, vNetId)
        override def getNPrios(c: ChannelRoutingInfo): Int = 2 * (topo.nLevels + 1)
        override def getPrio(srcC: ChannelRoutingInfo, nxtC: ChannelRoutingInfo,
          flow: FlowRoutingInfo): Int = {
          2 * MultiRucheMeshRouting.priority(topo, nxtC) +
            (if (isEscape(nxtC, flow.vNetId)) 1 else 0)
        }
        override val hardwareRouting = base.hardwareRouting
      }
    }
}
