package constellation.routing

import chisel3._
import constellation.topology.{GridNodeIdLayout, HierarchicalNodeIdLayout, TerminalNodeIdLayout}

object HardwareNodeId {
  def grid(id: UInt, layout: GridNodeIdLayout): (UInt, UInt) =
    (id(layout.xWidth - 1, 0), id(layout.width - 1, layout.xWidth))

  def terminalBase(id: UInt, layout: TerminalNodeIdLayout): UInt =
    id(layout.width - 1, 1)
  def terminalIsBase(id: UInt): Bool = id(0).asBool

  def hierarchyBase(id: UInt, layout: HierarchicalNodeIdLayout): UInt =
    id(layout.width - 1, layout.childPartWidth)
  def hierarchyIsChild(id: UInt, layout: HierarchicalNodeIdLayout): Bool =
    id(layout.childPartWidth - 1).asBool
  def hierarchySlot(id: UInt, layout: HierarchicalNodeIdLayout): UInt =
    id(layout.maxChildWidth + layout.attachmentWidth - 1, layout.maxChildWidth)
  def hierarchyLocal(id: UInt, layout: HierarchicalNodeIdLayout, width: Int): UInt =
    id(width - 1, 0)
}


/** Runtime channel metadata consumed by compact, topology-aware routing logic. */
case class HardwareRoutingChannel(
  src: UInt,
  dst: UInt,
  vc: UInt,
  nVC: Int,
  isIngress: Bool) {
  def copyWith(
    src: UInt = src,
    dst: UInt = dst,
    vc: UInt = vc,
    nVC: Int = nVC,
    isIngress: Bool = isIngress): HardwareRoutingChannel =
    HardwareRoutingChannel(src, dst, vc, nVC, isIngress)
}

case class HardwareRoutingFlow(
  vNetId: UInt,
  ingressNode: UInt,
  egressNode: UInt) {
  def copyWith(
    vNetId: UInt = vNetId,
    ingressNode: UInt = ingressNode,
    egressNode: UInt = egressNode): HardwareRoutingFlow =
    HardwareRoutingFlow(vNetId, ingressNode, egressNode)
}

/** An RTL equivalent of RoutingRelation.rel for structured runtime node IDs. */
trait HardwareRouting {
  def apply(
    source: HardwareRoutingChannel,
    next: HardwareRoutingChannel,
    flow: HardwareRoutingFlow): Bool
}

