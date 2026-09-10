package constellation.routing

import chisel3._
import chisel3.util._

import constellation.topology.{
  GridNodeIdLayout, HierarchicalNodeIdLayout, NodeIdLayout,
  HierarchicalTopology, Mesh2DLikePhysicalTopology, TerminalNodeIdLayout,
  TerminalRouter}
import HardwareNodeId._

/** Compact RTL implementations for routing relations over structured node IDs. */
object StructuredHardwareRouting {
  def meshDimensionOrdered(topo: Mesh2DLikePhysicalTopology, firstDim: Int): HardwareRouting = {
    val layout = NodeIdLayout(topo).asInstanceOf[GridNodeIdLayout]
    new HardwareRouting {
      def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
        flow: HardwareRoutingFlow): Bool = {
        val (nextX, nextY) = grid(next.dst, layout)
        val (nodeX, nodeY) = grid(next.src, layout)
        val (destX, destY) = grid(flow.egressNode, layout)
        val routeX = Mux(nodeX < nextX, destX >= nextX, destX <= nextX) && nextY === nodeY
        val routeY = Mux(nodeY < nextY, destY >= nextY, destY <= nextY) && nextX === nodeX
        if (firstDim == 0) Mux(destX =/= nodeX, routeX, routeY)
        else Mux(destY =/= nodeY, routeY, routeX)
      }
    }
  }

  def meshMinimal(topo: Mesh2DLikePhysicalTopology): HardwareRouting = {
    val layout = NodeIdLayout(topo).asInstanceOf[GridNodeIdLayout]
    new HardwareRouting {
      def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
        flow: HardwareRoutingFlow): Bool = {
        val (nextX, nextY) = grid(next.dst, layout)
        val (nodeX, nodeY) = grid(next.src, layout)
        val (destX, destY) = grid(flow.egressNode, layout)
        val routeX = Mux(nodeX < nextX, destX >= nextX,
          Mux(nodeX > nextX, destX <= nextX, true.B))
        val routeY = Mux(nodeY < nextY, destY >= nextY,
          Mux(nodeY > nextY, destY <= nextY, true.B))
        routeX && routeY
      }
    }
  }

  def meshWestFirst(topo: Mesh2DLikePhysicalTopology, base: HardwareRouting): HardwareRouting = {
    val layout = NodeIdLayout(topo).asInstanceOf[GridNodeIdLayout]
    new HardwareRouting {
      def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
        flow: HardwareRoutingFlow): Bool = {
        val (nextX, _) = grid(next.dst, layout)
        val (nodeX, _) = grid(next.src, layout)
        val (destX, _) = grid(flow.egressNode, layout)
        Mux(destX < nodeX, nextX === nodeX - 1.U, base(source, next, flow))
      }
    }
  }

  def meshNorthLast(topo: Mesh2DLikePhysicalTopology, base: HardwareRouting): HardwareRouting = {
    val layout = NodeIdLayout(topo).asInstanceOf[GridNodeIdLayout]
    new HardwareRouting {
      def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
        flow: HardwareRoutingFlow): Bool = {
        val (_, nextY) = grid(next.dst, layout)
        val (nodeX, nodeY) = grid(next.src, layout)
        val (destX, destY) = grid(flow.egressNode, layout)
        val minimal = base(source, next, flow)
        Mux(destY > nodeY && destX =/= nodeX,
          minimal && nextY =/= nodeY + 1.U,
          Mux(destY > nodeY, nextY === nodeY + 1.U, minimal))
      }
    }
  }

  def escape(
    escape: HardwareRouting,
    normal: HardwareRouting,
    nEscapeChannels: Int): HardwareRouting = new HardwareRouting {
    def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
      flow: HardwareRoutingFlow): Bool = {
      val nextIsEscape = next.vc < nEscapeChannels.U
      val sourceIsEscape = source.vc < nEscapeChannels.U
      val escapeNext = next.copyWith(nVC = nEscapeChannels)
      val normalNext = next.copyWith(
        vc = next.vc - nEscapeChannels.U,
        nVC = next.nVC - nEscapeChannels)
      val normalSource = source.copyWith(
        vc = source.vc - nEscapeChannels.U,
        nVC = source.nVC - nEscapeChannels)
      Mux(source.isIngress,
        Mux(nextIsEscape, escape(source, escapeNext, flow), normal(source, normalNext, flow)),
        Mux(nextIsEscape, escape(source, escapeNext, flow),
          Mux(!sourceIsEscape, normal(normalSource, normalNext, flow), false.B)))
    }
  }

  def nonblockingVirtualSubnetworks(
    base: HardwareRouting,
    n: Int,
    nDedicatedChannels: Int): HardwareRouting = new HardwareRouting {
    private def lowerVc(vc: UInt): UInt = Mux(vc < (n * nDedicatedChannels).U,
      vc % nDedicatedChannels.U,
      nDedicatedChannels.U + vc - (n * nDedicatedChannels).U)
    def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
      flow: HardwareRoutingFlow): Bool = {
      val able = next.vc >= (n * nDedicatedChannels).U ||
        next.vc / nDedicatedChannels.U === flow.vNetId
      val lowerSource = source.copyWith(
        vc = lowerVc(source.vc),
        nVC = source.nVC - n * nDedicatedChannels + nDedicatedChannels)
      val lowerNext = next.copyWith(
        vc = lowerVc(next.vc),
        nVC = next.nVC - n * nDedicatedChannels + nDedicatedChannels)
      able && base(lowerSource, lowerNext, flow.copyWith(vNetId = 0.U))
    }
  }

  def blockingVirtualSubnetworks(
    base: HardwareRouting,
    nDedicated: Int): HardwareRouting = new HardwareRouting {
    def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
      flow: HardwareRoutingFlow): Bool = {
      val offset = flow.vNetId * nDedicated.U
      val nextValid = next.vc >= offset
      val sourceValid = source.vc >= offset
      val lowerNext = next.copyWith(
        vc = next.vc - offset,
        nVC = next.nVC)
      val lowerSource = source.copyWith(
        vc = source.vc - offset,
        nVC = source.nVC)
      val routed = Mux(source.isIngress,
        base(source, lowerNext, flow.copyWith(vNetId = 0.U)),
        base(lowerSource, lowerNext, flow.copyWith(vNetId = 0.U)))
      nextValid && (source.isIngress || sourceValid) && routed
    }
  }

  def terminal(topo: TerminalRouter, base: HardwareRouting): HardwareRouting = {
    val layout = NodeIdLayout(topo).asInstanceOf[TerminalNodeIdLayout]
    new HardwareRouting {
      def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
        flow: HardwareRoutingFlow): Bool = {
        val currentBase = terminalIsBase(next.src)
        val nextBase = terminalIsBase(next.dst)
        val currentBaseId = terminalBase(next.src, layout)
        val nextBaseId = terminalBase(next.dst, layout)
        val flowBaseId = terminalBase(flow.egressNode, layout)
        val sourceIsTerminal = !source.isIngress && !terminalIsBase(source.src)
        val baseSource = source.copyWith(
          src = terminalBase(source.src, layout),
          dst = currentBaseId,
          vc = Mux(sourceIsTerminal, 0.U, source.vc),
          nVC = source.nVC,
          isIngress = source.isIngress || sourceIsTerminal)
        val baseNext = next.copyWith(src = currentBaseId, dst = nextBaseId)
        val baseFlow = flow.copyWith(egressNode = flowBaseId)
        val enterBase = source.isIngress && nextBase
        val atDestination = next.dst === flow.egressNode
        val traverseBase = currentBase && nextBase && currentBaseId =/= flowBaseId
        enterBase || atDestination || (traverseBase && base(baseSource, baseNext, baseFlow))
      }
    }
  }


  def hierarchy(
    topo: HierarchicalTopology,
    base: HardwareRouting,
    children: Seq[HardwareRouting]): HardwareRouting = {
    val layout = NodeIdLayout(topo).asInstanceOf[HierarchicalNodeIdLayout]
    require(children.nonEmpty && children.size == topo.children.size)

    def baseId(id: UInt): UInt = hierarchyBase(id, layout)
    def isChild(id: UInt): Bool = hierarchyIsChild(id, layout)
    def slot(id: UInt): UInt = hierarchySlot(id, layout)
    def localId(id: UInt, child: Int): UInt =
      hierarchyLocal(id, layout, layout.children(child).width)
    def selectSlot(which: UInt, values: Seq[Bool]): Bool =
      Mux1H(values.indices.map(i => (which === i.U) -> values(i)))
    def attachment(which: UInt): UInt = Mux1H(
      topo.children.indices.map { i =>
        (which === i.U) -> layout.base.encode(topo.children(i).src).U(layout.base.width.W)
      })

    new HardwareRouting {
      def apply(source: HardwareRoutingChannel, next: HardwareRoutingChannel,
        flow: HardwareRoutingFlow): Bool = {
        val currentIsChild = isChild(next.src)
        val nextIsChild = isChild(next.dst)
        val flowIsChild = isChild(flow.egressNode)
        val sourceIsChild = isChild(source.src)
        val currentBase = baseId(next.src)
        val nextBase = baseId(next.dst)
        val flowBase = baseId(flow.egressNode)
        val flowSlot = slot(flow.egressNode)
        val currentSlot = slot(next.src)
        val nextSlot = slot(next.dst)

        val baseSource = source.copyWith(
          src = baseId(source.src),
          dst = currentBase,
          isIngress = source.isIngress || sourceIsChild)
        val baseNext = next.copyWith(src = currentBase, dst = nextBase)
        val baseDirect = base(baseSource, baseNext, flow.copyWith(egressNode = flowBase))
        val childAttachment = attachment(flowSlot)
        val baseToChild = base(baseSource, baseNext,
          flow.copyWith(egressNode = childAttachment))

        val childRoutes = topo.children.indices.map { i =>
          val childLayout = layout.children(i)
          val childExit = childLayout.encode(topo.children(i).dst).U(childLayout.width.W)
          val sameDestinationChild = flowSlot === i.U
          val targetThisChild = flowIsChild && sameDestinationChild
          val target = Mux(targetThisChild, localId(flow.egressNode, i), childExit)
          val childSource = source.copyWith(
            src = localId(source.src, i),
            dst = localId(next.src, i),
            isIngress = source.isIngress || !sourceIsChild)
          val childNext = next.copyWith(
            src = localId(next.src, i),
            dst = localId(next.dst, i))
          val childFlow = flow.copyWith(egressNode = target)
          val atExitForOtherChild =
            !targetThisChild && localId(next.src, i) === childExit
          children(i)(childSource, childNext, childFlow) && !atExitForOtherChild
        }
        val childRoute = selectSlot(currentSlot, childRoutes)
        val currentAtChildExit = selectSlot(currentSlot,
          topo.children.indices.map { i =>
            localId(next.src, i) ===
              layout.children(i).encode(topo.children(i).dst).U(layout.children(i).width.W)
          })

        val baseToBaseDestination =
          !currentIsChild && !flowIsChild && !nextIsChild && baseDirect
        val baseTowardChild =
          !currentIsChild && flowIsChild && !nextIsChild &&
            currentBase =/= childAttachment && baseToChild
        val enterChild =
          !currentIsChild && flowIsChild && nextIsChild && nextSlot === flowSlot
        val leaveChildForBase =
          currentIsChild && !flowIsChild && !nextIsChild
        val childTowardBase =
          currentIsChild && !flowIsChild && nextIsChild &&
            childRoute && !currentAtChildExit
        val leaveChildForSibling =
          currentIsChild && flowIsChild && !nextIsChild && currentSlot =/= flowSlot
        val routeWithinChild =
          currentIsChild && flowIsChild && nextIsChild && childRoute

        baseToBaseDestination || baseTowardChild || enterChild ||
          leaveChildForBase || childTowardBase ||
          leaveChildForSibling || routeWithinChild
      }
    }
  }
}

