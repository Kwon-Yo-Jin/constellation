package constellation.router

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

import constellation.channel._
import constellation.routing.{FlowRoutingInfo, RoutingRelation}
import constellation.noc.{HasNoCParams}

case class UserRouterParams(
  // Payload width. Must match payload width on all channels attached to this routing node
  payloadBits: Int = 64,
  // Combines SA and ST stages (removes pipeline register)
  combineSAST: Boolean = false,
  // Combines RC and VA stages (removes pipeline register)
  combineRCVA: Boolean = false,
  // Adds combinational path from SA to VA
  coupleSAVA: Boolean = false,
  vcAllocator: VCAllocatorParams => Parameters => VCAllocator = (vP) => (p) => new RotatingSingleVCAllocator(vP)(p)
)

case class RouterParams(
  nodeId: Int,
  nIngress: Int,
  nEgress: Int,
  user: UserRouterParams
)

/** Parameters which can change the generated hardware shape of a channel.
  * Topology-local identifiers and flow sets are deliberately excluded: those
  * are selected from the runtime node ID instead.
  */
case class ChannelHardwareShape(
  payloadBits: Int,
  nVirtualChannels: Int,
  bufferSizes: Seq[Int],
  traversableVCs: Seq[Boolean],
  srcSpeedup: Int,
  destSpeedup: Int,
  useOutputQueues: Boolean,
  unifiedBuffer: Boolean)

object ChannelHardwareShape {
  def apply(p: BaseChannelParams): ChannelHardwareShape = p match {
    case c: ChannelParams => ChannelHardwareShape(
      c.payloadBits,
      c.nVirtualChannels,
      c.virtualChannelParams.map(_.bufferSize),
      c.virtualChannelParams.map(_.traversable),
      c.srcSpeedup,
      c.destSpeedup,
      c.useOutputQueues,
      c.unifiedBuffer)
    case _ => ChannelHardwareShape(
      p.payloadBits,
      p.nVirtualChannels,
      Nil,
      Seq(p.traversable),
      p.srcSpeedup,
      p.destSpeedup,
      false,
      false)
  }
}

case class RouterHardwareShape(
  in: Seq[ChannelHardwareShape],
  out: Seq[ChannelHardwareShape],
  ingress: Seq[ChannelHardwareShape],
  egress: Seq[ChannelHardwareShape],
  payloadBits: Int,
  combineSAST: Boolean,
  combineRCVA: Boolean,
  coupleSAVA: Boolean,
  allocatorClass: String)

case class InputUnitRoutingShape(
  inputVirtualChannels: Int,
  outputVirtualChannels: Seq[Int],
  egressVirtualChannels: Seq[Int],
  combineRCVA: Boolean,
  combineSAST: Boolean)

case class EgressUnitRoutingShape(
  virtualChannels: Int,
  coupleSAVA: Boolean,
  combineSAST: Boolean)

case class RouterRoutingContext(
  nodeId: Int,
  inParams: Seq[ChannelParams],
  outParams: Seq[ChannelParams],
  ingressParams: Seq[IngressChannelParams],
  egressParams: Seq[EgressChannelParams],
  user: UserRouterParams = UserRouterParams()
) extends HasRouterInputParams with HasRouterOutputParams {
  def hasSamePolicyShape(that: RouterRoutingContext): Boolean = {
    inParams.map(_.nVirtualChannels) == that.inParams.map(_.nVirtualChannels) &&
    outParams.map(_.nVirtualChannels) == that.outParams.map(_.nVirtualChannels) &&
    ingressParams.size == that.ingressParams.size &&
    egressParams.size == that.egressParams.size
  }

  def hardwareShape: RouterHardwareShape = RouterHardwareShape(
    inParams.map(ChannelHardwareShape(_)),
    outParams.map(ChannelHardwareShape(_)),
    ingressParams.map(ChannelHardwareShape(_)),
    egressParams.map(ChannelHardwareShape(_)),
    user.payloadBits,
    user.combineSAST,
    user.combineRCVA,
    user.coupleSAVA,
    user.vcAllocator.getClass.getName)

  def inputUnitRoutingShape(cParam: BaseChannelParams): InputUnitRoutingShape =
    InputUnitRoutingShape(
      cParam.nVirtualChannels,
      outParams.map(_.nVirtualChannels),
      egressParams.map(_.nVirtualChannels),
      user.combineRCVA,
      user.combineSAST)

  def egressUnitRoutingShape(cParam: EgressChannelParams): EgressUnitRoutingShape =
    EgressUnitRoutingShape(
      cParam.nVirtualChannels,
      user.coupleSAVA && nAllInputs == 1,
      user.combineSAST)
}

object RouterRoutingContext {
  def orderedFlows(flows: Set[FlowRoutingInfo]): Seq[FlowRoutingInfo] =
    flows.toSeq.sortBy(f => (
      f.ingressId, f.egressId, f.vNetId, f.ingressNode,
      f.ingressNodeId, f.egressNode, f.egressNodeId, f.fifo))
}

trait HasRouterOutputParams {
  def outParams: Seq[ChannelParams]
  def egressParams: Seq[EgressChannelParams]

  def allOutParams = outParams ++ egressParams

  def nOutputs = outParams.size
  def nEgress = egressParams.size
  def nAllOutputs = allOutParams.size
}

trait HasRouterInputParams {
  def inParams: Seq[ChannelParams]
  def ingressParams: Seq[IngressChannelParams]

  def allInParams = inParams ++ ingressParams

  def nInputs = inParams.size
  def nIngress = ingressParams.size
  def nAllInputs = allInParams.size
}

trait HasRouterParams
{
  def routerParams: RouterParams
  def nodeId = routerParams.nodeId
  def payloadBits = routerParams.user.payloadBits
}

class DebugBundle(val nIn: Int) extends Bundle {
  val va_stall = Vec(nIn, UInt())
  val sa_stall = Vec(nIn, UInt())
}

class Router(
  val routerParams: RouterParams,
  preDiplomaticInParams: Seq[ChannelParams],
  preDiplomaticIngressParams: Seq[IngressChannelParams],
  outDests: Seq[Int],
  egressIds: Seq[Int],
  routingContexts: Seq[RouterRoutingContext],
  topologyContext: RouterRoutingContext
)(implicit p: Parameters) extends LazyModule with HasNoCParams with HasRouterParams {
  val allPreDiplomaticInParams = preDiplomaticInParams ++ preDiplomaticIngressParams

  val destNodes = preDiplomaticInParams.map(u => ChannelDestNode(u))
  val sourceNodes = outDests.map(u => ChannelSourceNode(u))
  val ingressNodes = preDiplomaticIngressParams.map(u => IngressChannelDestNode(u))
  val egressNodes = egressIds.map(u => EgressChannelSourceNode(u))

  // The diplomatic boundary can include inactive padding ports so every
  // router in a NoC has the same module interface. Keep the real-prefix views
  // for physical topology and terminal wiring; the remaining nodes are tied
  // off by NoC.
  val realDestNodes = destNodes.take(topologyContext.inParams.size)
  val realSourceNodes = sourceNodes.take(topologyContext.outParams.size)
  val realIngressNodes = ingressNodes.take(topologyContext.ingressParams.size)
  val realEgressNodes = egressNodes.take(topologyContext.egressParams.size)

  val debugNode = BundleBridgeSource(() => new DebugBundle(allPreDiplomaticInParams.size))
  val ctrlNode = if (hasCtrl) Some(BundleBridgeSource(() => new RouterCtrlBundle)) else None

  /** Runtime node ID input. The static routerParams.nodeId remains only for
    * topology construction and elaboration-time validation.
    */
  private val nodeIdNexusNode = BundleBroadcast[UInt]()
  private val nodeIdSinkNode = BundleBridgeSink[UInt](Some(() => UInt(nodeIdBits.W)))
  val nodeIdNode = nodeIdSinkNode := nodeIdNexusNode := BundleBridgeNameNode[UInt]("node_id")

  // Metadata consumers (graph/adjacency artefacts) must not force the module
  // of a CloneLazyModule. The topology context is equivalent to the negotiated
  // edge parameters used by this router.
  def inParams = topologyContext.inParams
  def outParams = topologyContext.outParams
  def ingressParams = topologyContext.ingressParams
  def egressParams = topologyContext.egressParams

  lazy val module = new LazyModuleImp(this) with HasRouterInputParams with HasRouterOutputParams {

    val (io_in, edgesIn) = destNodes.map(_.in(0)).unzip
    val (io_out, edgesOut) = sourceNodes.map(_.out(0)).unzip
    val (io_ingress, edgesIngress) = ingressNodes.map(_.in(0)).unzip
    val (io_egress, edgesEgress) = egressNodes.map(_.out(0)).unzip
    val io_debug = debugNode.out(0)._1
    val runtimeNodeId = nodeIdSinkNode.bundle

    require(runtimeNodeId.getWidth == nodeIdBits)
    dontTouch(runtimeNodeId)

    val inParams = edgesIn.map(_.cp)
    val outParams = edgesOut.map(_.cp)
    val ingressParams = edgesIngress.map(_.cp)
    val egressParams = edgesEgress.map(_.cp)

    allOutParams.foreach(u => require(u.srcId == nodeId && u.payloadBits == routerParams.user.payloadBits))
    allInParams.foreach(u => require(u.destId == nodeId && u.payloadBits == routerParams.user.payloadBits))

    require(nIngress == routerParams.nIngress)
    require(nEgress == routerParams.nEgress)
    require(nAllInputs >= 1)
    require(nAllOutputs >= 1)
    require(nodeId < (1 << nodeIdBits))

    val input_monitors = (io_in zip inParams).zipWithIndex.map {
      case ((in, param), portId) =>
        val monitor = Module(new NoCMonitor(param, routingContexts))
          .suggestName(s"input_monitor_$portId")
        monitor.io.node_id := runtimeNodeId
        monitor.io.port_id := portId.U
        monitor.io.in := in
        monitor
    }
    val input_units = inParams.zipWithIndex.map { case (u,i) =>
      Module(new InputUnit(u, outParams, egressParams,
        routerParams.user.combineRCVA, routerParams.user.combineSAST,
        routingContexts))
        .suggestName(s"input_unit_$i") }
    val ingress_units = ingressParams.zipWithIndex.map { case (u,i) =>
      Module(new IngressUnit(u, outParams, egressParams,
        routerParams.user.combineRCVA, routerParams.user.combineSAST,
        routingContexts))
        .suggestName(s"ingress_unit_${i+nInputs}") }
    val all_input_units = input_units ++ ingress_units

    val output_units = outParams.zipWithIndex.map { case (u,i) =>
      Module(new OutputUnit(inParams, ingressParams, u))
        .suggestName(s"output_unit_$i")}
    val egress_units = egressParams.zipWithIndex.map { case (u,i) =>
      Module(new EgressUnit(routerParams.user.coupleSAVA && all_input_units.size == 1,
        routerParams.user.combineSAST,
        inParams, ingressParams, u, routingContexts))
        .suggestName(s"egress_unit_${i+nOutputs}")}
    val all_output_units = output_units ++ egress_units

    val switch = Module(new Switch(inParams, outParams, ingressParams, egressParams))
    val switch_allocator = Module(new SwitchAllocator(inParams, outParams, ingressParams, egressParams))
    val vc_allocator = Module(routerParams.user.vcAllocator(
      VCAllocatorParams(routingContexts, inParams, outParams, ingressParams, egressParams)
    )(p))
    val route_computer = Module(new RouteComputer(
      routingContexts, inParams, outParams, ingressParams, egressParams))

    all_input_units.foreach(_.io.node_id := runtimeNodeId)
    input_units.zipWithIndex.foreach { case (u, i) => u.io.port_id := i.U }
    ingress_units.zipWithIndex.foreach { case (u, i) => u.io.port_id := i.U }
    egress_units.foreach(_.io.node_id := runtimeNodeId)
    egress_units.zipWithIndex.foreach { case (u, i) => u.io.port_id := i.U }
    vc_allocator.io.node_id := runtimeNodeId
    route_computer.io.node_id := runtimeNodeId


    val fires_count = WireInit(PopCount(vc_allocator.io.req.map(_.fire)))
    dontTouch(fires_count)


    (io_in      zip input_units  ).foreach { case (i,u) => u.io.in <> i }
    (io_ingress zip ingress_units).foreach { case (i,u) => u.io.in <> i.flit }
    (output_units zip io_out   ).foreach { case (u,o) => o <> u.io.out }
    (egress_units zip io_egress).foreach { case (u,o) => o.flit <> u.io.out }
    (route_computer.io.req zip all_input_units).foreach {
      case (i,u) => i <> u.io.router_req }
    (all_input_units zip route_computer.io.resp).foreach {
      case (u,o) => u.io.router_resp <> o }

    (vc_allocator.io.req zip all_input_units).foreach {
      case (i,u) => i <> u.io.vcalloc_req }
    (all_input_units zip vc_allocator.io.resp).foreach {
      case (u,o) => u.io.vcalloc_resp <> o }


    (all_output_units zip vc_allocator.io.out_allocs).foreach {
      case (u,a) => u.io.allocs <> a }
    (vc_allocator.io.channel_status zip all_output_units).foreach {
      case (a,u) => a := u.io.channel_status }

    all_input_units.foreach(in => all_output_units.zipWithIndex.foreach { case (out,outIdx) =>
      in.io.out_credit_available(outIdx) := out.io.credit_available
    })
    (all_input_units zip switch_allocator.io.req).foreach {
      case (u,r) => r <> u.io.salloc_req }
    (all_output_units zip switch_allocator.io.credit_alloc).foreach {
      case (u,a) => u.io.credit_alloc := a }

    (switch.io.in zip all_input_units).foreach {
      case (i,u) => i <> u.io.out }
    (all_output_units zip switch.io.out).foreach {
      case (u,o) => u.io.in <> o }
    switch.io.sel := (if (routerParams.user.combineSAST) {
      switch_allocator.io.switch_sel
    } else {
      RegNext(switch_allocator.io.switch_sel)
    })

    if (hasCtrl) {
      val io_ctrl = ctrlNode.get.out(0)._1
      val ctrl = Module(new RouterControlUnit(inParams, outParams, ingressParams, egressParams))
      io_ctrl <> ctrl.io.ctrl
      (all_input_units   zip ctrl.io.in_block  ).foreach { case (l,r) => l.io.block := r }
      (all_input_units   zip ctrl.io.in_fire   ).foreach { case (l,r) => r := l.io.out.map(_.valid) }
    } else {
      input_units.foreach(_.io.block := false.B)
      ingress_units.foreach(_.io.block := false.B)
    }

    (io_debug.va_stall zip all_input_units.map(_.io.debug.va_stall)).map { case (l,r) => l := r }
    (io_debug.sa_stall zip all_input_units.map(_.io.debug.sa_stall)).map { case (l,r) => l := r }

    val debug_tsc = RegInit(0.U(64.W))
    debug_tsc := debug_tsc + 1.U
    val debug_sample = RegInit(0.U(64.W))
    debug_sample := debug_sample + 1.U
    val sample_rate = PlusArg("noc_util_sample_rate", width=20)
    when (debug_sample === sample_rate - 1.U) { debug_sample := 0.U }

    def sample(fire: Bool, edgeFormat: String, edgeArgs: Bits*) = {
      val util_ctr = RegInit(0.U(64.W))
      val fired = RegInit(false.B)
      util_ctr := util_ctr + fire
      fired := fired || fire
      when (sample_rate =/= 0.U && debug_sample === sample_rate - 1.U && fired) {
        val fmtStr = s"nocsample %d $edgeFormat %d\n"
        printf(fmtStr, (Seq(debug_tsc) ++ edgeArgs ++ Seq(util_ctr)): _*);
        fired := fire
      }
    }

    val localContext = RouterRoutingContext(
      nodeId, inParams, outParams, ingressParams, egressParams, routerParams.user)
    val compatibleRouterContexts = routingContexts.filter(_.hasSamePolicyShape(localContext))
    require(compatibleRouterContexts.nonEmpty)

    destNodes.map(_.in(0)).zipWithIndex.foreach { case ((in, _), portId) => in.flit.map { f =>
      val sourceId = PriorityMux(compatibleRouterContexts.map { context =>
        (runtimeNodeId === context.nodeId.U) -> context.inParams(portId).srcId.U(nodeIdBits.W)
      })
      sample(f.fire, "%d %d", sourceId, runtimeNodeId)
    } }
    ingressNodes.map(_.in(0)).zipWithIndex.foreach { case ((in, _), portId) =>
      val ingressId = PriorityMux(compatibleRouterContexts.map { context =>
        (runtimeNodeId === context.nodeId.U) -> context.ingressParams(portId).ingressId.U(ingressIdBits.W)
      })
      sample(in.flit.fire, "i%d %d", ingressId, runtimeNodeId)
    }
    egressNodes.map(_.out(0)).zipWithIndex.foreach { case ((out, _), portId) =>
      val egressId = PriorityMux(compatibleRouterContexts.map { context =>
        (runtimeNodeId === context.nodeId.U) -> context.egressParams(portId).egressId.U(egressIdBits.W)
      })
      sample(out.flit.fire, "%d e%d", runtimeNodeId, egressId)
    }

  }
}
