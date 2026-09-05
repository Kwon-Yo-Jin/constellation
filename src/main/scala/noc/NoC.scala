package constellation.noc

import scala.collection.mutable

import chisel3._
import chisel3.util._


import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{BundleBridgeEphemeralNode, BundleBridgeNexus, BundleBridgeSink, CloneLazyModule, InModuleBody, LazyModule, LazyModuleImp}
import freechips.rocketchip.util.ElaborationArtefacts
import freechips.rocketchip.prci._
import constellation.router._
import constellation.channel._
import constellation.routing.{ChannelRoutingInfo, FlowRoutingInfo, RoutingRelation}
import constellation.topology.{PhysicalTopology, UnidirectionalLine}


class NoCTerminalIO(
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle {
  val ingress = MixedVec(ingressParams.map { u => Flipped(new IngressChannel(u)) })
  val egress = MixedVec(egressParams.map { u => new EgressChannel(u) })
}

class NoC(nocParams: NoCParams)(implicit p: Parameters) extends LazyModule {

  override def shouldBeInlined = nocParams.inlineNoC

  val internalParams = InternalNoCParams(nocParams)
  val allChannelParams = internalParams.channelParams
  val allIngressParams = internalParams.ingressParams
  val allEgressParams = internalParams.egressParams
  val allRouterParams = internalParams.routerParams

  val iP = p.alterPartial({ case InternalNoCKey => internalParams })
  val nNodes = nocParams.topology.nNodes
  val nocName = nocParams.nocName
  val skipValidationChecks = nocParams.skipValidationChecks

  val clockSourceNodes = Seq.tabulate(nNodes) { i => ClockSourceNode(Seq(ClockSourceParameters())) }
  val router_sink_domains = Seq.tabulate(nNodes) { i =>
    val router_sink_domain = LazyModule(new ClockSinkDomain(ClockSinkParameters(
      name = Some(s"${nocName}_router_$i")
    )))
    router_sink_domain.clockNode := clockSourceNodes(i)
    router_sink_domain
  }

  private val allActualRouterContexts = Seq.tabulate(nNodes) { i =>
    val inParams = allChannelParams.filter(_.destId == i).map(
      _.copy(payloadBits=allRouterParams(i).user.payloadBits)
    )
    val outParams = allChannelParams.filter(_.srcId == i).map(
      _.copy(payloadBits=allRouterParams(i).user.payloadBits)
    )
    val ingressParams = allIngressParams.filter(_.destId == i).map(
      _.copy(payloadBits=allRouterParams(i).user.payloadBits)
    )
    val egressParams = allEgressParams.filter(_.srcId == i).map(
      _.copy(payloadBits=allRouterParams(i).user.payloadBits)
    )
    RouterRoutingContext(
      i, inParams, outParams, ingressParams, egressParams, allRouterParams(i).user)
  }
  private val actualRouterContexts = allActualRouterContexts.filter { context =>
    val noIn = context.allInParams.isEmpty
    val noOut = context.allOutParams.isEmpty
    if (noIn || noOut) {
      println(s"Constellation WARNING: $nocName router ${context.nodeId} seems to be unused, it will not be generated")
      false
    } else {
      true
    }
  }

  /** Give every router the largest port envelope required by this topology.
    * Real ports are kept as a prefix; inactive suffix ports are connected to
    * local tie-offs below. This is topology-independent: a mesh naturally
    * produces four network ports, while trees, rings, hierarchical networks,
    * and custom topologies use their own maximum directed degree.
    */
  private val maxNetworkInputs = (0 until nNodes).map { dst =>
    (0 until nNodes).count(src => src != dst && nocParams.topology.topo(src, dst))
  }.max
  private val maxNetworkOutputs = (0 until nNodes).map { src =>
    (0 until nNodes).count(dst => src != dst && nocParams.topology.topo(src, dst))
  }.max
  private val maxTerminalInputs = actualRouterContexts.map(_.ingressParams.size).max
  private val maxTerminalOutputs = actualRouterContexts.map(_.egressParams.size).max

  private def channelShapeIgnoringActivity(p: ChannelParams): ChannelHardwareShape =
    ChannelHardwareShape(p).copy(traversableVCs = Seq.fill(p.nVirtualChannels)(true))

  private def routerBodyShape(context: RouterRoutingContext): RouterHardwareShape =
    context.hardwareShape.copy(in = Nil, out = Nil, ingress = Nil, egress = Nil)

  // Port padding is safe when the actual channels agree on their physical
  // implementation and every router uses the same pipeline/allocator body.
  // Networks with deliberately heterogeneous widths or speedups retain the
  // exact-shape cloning fallback.
  private val useUniformRouterEnvelope =
    allChannelParams.map(channelShapeIgnoringActivity).distinct.size <= 1 &&
      actualRouterContexts.map(routerBodyShape).distinct.size == 1

  private val networkTemplate = allChannelParams.headOption.orElse {
    Option.when(maxNetworkInputs + maxNetworkOutputs > 0) {
      ChannelParams(
        srcId = 0,
        destId = 0,
        payloadBits = allRouterParams.head.user.payloadBits,
        user = nocParams.channelParamGen(0, 0))
    }
  }
  private val networkVCFlowUnions: Seq[Set[FlowRoutingInfo]] = networkTemplate.map { template =>
    (0 until template.nVirtualChannels).map { vc =>
      allChannelParams.flatMap(_.virtualChannelParams.lift(vc).toSeq)
        .flatMap(_.possibleFlows).toSet
    }
  }.getOrElse(Nil)
  private val ingressFlowUnion = allIngressParams.flatMap(_.possibleFlows).toSet
  private val egressFlowUnion = allEgressParams.flatMap(_.possibleFlows).toSet

  private def withChannelIds(
    p: ChannelParams,
    srcId: Int,
    destId: Int,
    possibleFlows: Seq[Set[FlowRoutingInfo]]): ChannelParams = p.copy(
      srcId = srcId,
      destId = destId,
      virtualChannelParams = p.virtualChannelParams.zipWithIndex.map { case (vc, i) =>
        vc.copy(src = srcId, dst = destId, possibleFlows = possibleFlows(i))
      })

  private def dummyChannel(nodeId: Int): ChannelParams = withChannelIds(
    networkTemplate.get,
    nodeId,
    nodeId,
    networkTemplate.get.virtualChannelParams.map(_ => Set.empty[FlowRoutingInfo]))

  private def dummyIngress(nodeId: Int): IngressChannelParams =
    allIngressParams.head.copy(
      ingressId = 0,
      destId = nodeId,
      possibleFlows = Set.empty,
      vNetId = 0,
      payloadBits = allRouterParams(nodeId).user.payloadBits)

  private def dummyEgress(nodeId: Int): EgressChannelParams =
    allEgressParams.head.copy(
      egressId = 0,
      srcId = nodeId,
      possibleFlows = Set.empty,
      payloadBits = allRouterParams(nodeId).user.payloadBits)

  private def paddedContext(context: RouterRoutingContext): RouterRoutingContext = {
    val nodeId = context.nodeId
    context.copy(
      inParams = context.inParams ++ Seq.fill(maxNetworkInputs - context.inParams.size)(dummyChannel(nodeId)),
      outParams = context.outParams ++ Seq.fill(maxNetworkOutputs - context.outParams.size)(dummyChannel(nodeId)),
      ingressParams = context.ingressParams ++
        Seq.fill(maxTerminalInputs - context.ingressParams.size)(dummyIngress(nodeId)),
      egressParams = context.egressParams ++
        Seq.fill(maxTerminalOutputs - context.egressParams.size)(dummyEgress(nodeId)))
  }

  private val routerRoutingContexts = if (useUniformRouterEnvelope) {
    actualRouterContexts.map(paddedContext)
  } else {
    actualRouterContexts
  }

  private def hardwareContext(context: RouterRoutingContext): RouterRoutingContext = {
    if (!useUniformRouterEnvelope) {
      context
    } else {
      context.copy(
        inParams = context.inParams.map(p => withChannelIds(
          p, p.srcId, p.destId, networkVCFlowUnions)),
        outParams = context.outParams.map(p => withChannelIds(
          p, p.srcId, p.destId, networkVCFlowUnions)),
        ingressParams = context.ingressParams.map(_.copy(possibleFlows = ingressFlowUnion)),
        egressParams = context.egressParams.map(_.copy(possibleFlows = egressFlowUnion)))
    }
  }
  private val routerHardwareContexts = routerRoutingContexts.map { context =>
    context.nodeId -> hardwareContext(context)
  }.toMap

  // As in Rocket Chip's CloneTileAttachParams, elaborate one prototype for
  // each true hardware shape and clone all matching instances.
  private val routerPayloadWidths = allRouterParams.map(_.user.payloadBits).distinct
  private val hasUniformUnconvertedPayload = routerPayloadWidths.size == 1 && {
    val width = routerPayloadWidths.head
    allIngressParams.forall(_.payloadBits == width) &&
      allEgressParams.forall(_.payloadBits == width)
  }
  private val routerPrototypes =
    mutable.LinkedHashMap[(RouterHardwareShape, Option[Int]), Router]()

  val routers = actualRouterContexts.map { topologyContext =>
    val i = topologyContext.nodeId
    val context = routerHardwareContexts(i)
    router_sink_domains(i) {
      val candidate = new Router(
        routerParams = allRouterParams(i).copy(
          nIngress = context.ingressParams.size,
          nEgress = context.egressParams.size),
        preDiplomaticInParams = context.inParams,
        preDiplomaticIngressParams = context.ingressParams,
        outDests = context.outParams.map(_.destId),
        egressIds = context.egressParams.map(_.egressId),
        routingContexts = routerRoutingContexts,
        topologyContext = topologyContext
      )(iP)
      val cloneKey = context.hardwareShape ->
        Option.when(!useUniformRouterEnvelope && !hasUniformUnconvertedPayload)(i)
      routerPrototypes.get(cloneKey) match {
        case Some(prototype) =>
          CloneLazyModule(candidate, prototype)
        case None =>
          val prototype = LazyModule(candidate)
          routerPrototypes(cloneKey) = prototype
          prototype
      }
    }
  }
  if (useUniformRouterEnvelope) {
    println(
      s"Constellation: $nocName uniform router envelope is " +
        s"$maxNetworkInputs/$maxNetworkOutputs network and " +
        s"$maxTerminalInputs/$maxTerminalOutputs terminal input/output ports")
  }
  println(s"Constellation: $nocName uses ${routerPrototypes.size} router hardware shapes for ${routers.size} routers")

  // Keep the node ID as an RTL input to the router so otherwise homogeneous
  // routers can eventually share one module definition. This follows the
  // hart-ID distribution pattern used by Rocket Chip tiles.
  private val generatedRouterIds = routers.map(_.nodeId)
  private val routerNodeIdNodes = generatedRouterIds.map { i =>
    i -> BundleBridgeEphemeralNode[UInt]()
  }.toMap
  private val routerNodeIdNexusNode = LazyModule(new BundleBridgeNexus[UInt](
    inputFn = BundleBridgeNexus.orReduction[UInt](registered = false) _,
    outputFn = (prefix: UInt, n: Int) => {
      require(n == generatedRouterIds.size)
      generatedRouterIds.map { i =>
        dontTouch(prefix | i.U(log2Ceil(nNodes).W))
      }
    },
    default = Some(() => 0.U(log2Ceil(nNodes).W)),
    inputRequiresOutput = true,
    shouldBeInlined = false
  )(iP)).node

  generatedRouterIds.foreach { i =>
    routerNodeIdNodes(i) :*= routerNodeIdNexusNode
  }
  routers.foreach { r =>
    r.nodeIdNode := routerNodeIdNodes(r.nodeId)
  }

  val ingressNodes = allIngressParams.map { u => IngressChannelSourceNode(u.destId) }
  val egressNodes = allEgressParams.map { u => EgressChannelDestNode(u) }

  private case class RouterTieOffNodes(
    channelInputs: Seq[ChannelSourceNode],
    channelOutputs: Seq[ChannelDestNode],
    terminalInputs: Seq[IngressChannelSourceNode],
    terminalOutputs: Seq[EgressChannelDestNode])

  // Bind every inactive suffix port to a local diplomatic endpoint. Signal
  // values are driven/consumed in Impl so these ports remain inert in RTL.
  private val routerTieOffNodes = routers.map { router =>
    val context = routerHardwareContexts(router.nodeId)
    val channelInputs = router.destNodes.drop(router.realDestNodes.size).map { _ =>
      ChannelSourceNode(router.nodeId)
    }
    val channelOutputs = router.sourceNodes.drop(router.realSourceNodes.size).zipWithIndex.map {
      case (_, offset) =>
        val portId = router.realSourceNodes.size + offset
        ChannelDestNode(context.outParams(portId))
    }
    val terminalInputs = router.ingressNodes.drop(router.realIngressNodes.size).map { _ =>
      IngressChannelSourceNode(router.nodeId)
    }
    val terminalOutputs = router.egressNodes.drop(router.realEgressNodes.size).zipWithIndex.map {
      case (_, offset) =>
        val portId = router.realEgressNodes.size + offset
        EgressChannelDestNode(context.egressParams(portId))
    }
    router_sink_domains(router.nodeId) {
      implicit val p: Parameters = iP
      router.destNodes.drop(router.realDestNodes.size).zip(channelInputs).foreach {
        case (dest, source) =>
        dest := source
      }
      router.sourceNodes.drop(router.realSourceNodes.size).zip(channelOutputs).foreach {
        case (source, dest) =>
          dest := source
      }
      router.ingressNodes.drop(router.realIngressNodes.size).zip(terminalInputs).foreach {
        case (dest, source) =>
        dest := source
      }
      router.egressNodes.drop(router.realEgressNodes.size).zip(terminalOutputs).foreach {
        case (source, dest) =>
          dest := source
      }
    }
    RouterTieOffNodes(channelInputs, channelOutputs, terminalInputs, terminalOutputs)
  }

  // Generate channels between routers diplomatically
  Seq.tabulate(nNodes, nNodes) { case (i, j) => if (i != j) {
    val routerI = routers.find(_.nodeId == i)
    val routerJ = routers.find(_.nodeId == j)
    if (routerI.isDefined && routerJ.isDefined) {
      val sourceNodes: Seq[ChannelSourceNode] = routerI.get.realSourceNodes.filter(_.destId == j)
      val destNodes: Seq[ChannelDestNode] = routerJ.get.realDestNodes.filter(_.destParams.srcId == i)
      require (sourceNodes.size == destNodes.size)
      (sourceNodes zip destNodes).foreach { case (src, dst) =>
        val channelParam = allChannelParams.find(c => c.srcId == i && c.destId == j).get
        router_sink_domains(j) {
          implicit val p: Parameters = iP
          (dst
            := ChannelWidthWidget(routerJ.get.payloadBits, routerI.get.payloadBits)
            := channelParam.channelGen(p)(src)
          )
        }
      }
    }
  }}

  // Generate terminal channels diplomatically
  routers.foreach { dst => router_sink_domains(dst.nodeId) {
    implicit val p: Parameters = iP
    dst.realIngressNodes.foreach(n => {
      val ingressId = n.destParams.ingressId
      require(dst.payloadBits <= allIngressParams(ingressId).payloadBits)
      (n
        := IngressWidthWidget(dst.payloadBits, allIngressParams(ingressId).payloadBits)
        := ingressNodes(ingressId)
      )
    })
    dst.realEgressNodes.foreach(n => {
      val egressId = n.egressId
      require(dst.payloadBits <= allEgressParams(egressId).payloadBits)
      (egressNodes(egressId)
        := EgressWidthWidget(allEgressParams(egressId).payloadBits, dst.payloadBits)
        := n
      )
    })
  }}

  val debugNodes = routers.map { r =>
    val sink = BundleBridgeSink[DebugBundle]()
    sink := r.debugNode
    sink
  }
  val ctrlNodes = if (nocParams.hasCtrl) {
    (0 until nNodes).map { i =>
      routers.find(_.nodeId == i).map { r =>
        val sink = BundleBridgeSink[RouterCtrlBundle]()
        sink := r.ctrlNode.get
        sink
      }
    }
  } else {
    Nil
  }

  println(s"Constellation: $nocName Finished parameter validation")
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    println(s"Constellation: $nocName Starting NoC RTL generation")
    val io = IO(new NoCTerminalIO(allIngressParams, allEgressParams)(iP) {
      val router_clocks = Vec(nNodes, Input(new ClockBundle(ClockBundleParameters())))
      val router_ctrl = if (nocParams.hasCtrl) Vec(nNodes, new RouterCtrlBundle) else Nil
    })

    (io.ingress zip ingressNodes.map(_.out(0)._1)).foreach { case (l,r) => r <> l }
    (io.egress  zip egressNodes .map(_.in (0)._1)).foreach { case (l,r) => l <> r }
    (io.router_clocks zip clockSourceNodes.map(_.out(0)._1)).foreach { case (l,r) => l <> r }

    routerTieOffNodes.foreach { tieOffs =>
      tieOffs.channelInputs.foreach { source =>
        val out = source.out(0)._1
        out.flit.foreach { flit =>
          flit.valid := false.B
          flit.bits := DontCare
        }
      }
      tieOffs.channelOutputs.foreach { dest =>
        val in = dest.in(0)._1
        in.credit_return := 0.U
        in.vc_free := 0.U
        assert(!in.flit.map(_.valid).reduce(_ || _))
      }
      tieOffs.terminalInputs.foreach { source =>
        val out = source.out(0)._1.flit
        out.valid := false.B
        out.bits := DontCare
      }
      tieOffs.terminalOutputs.foreach { dest =>
        val in = dest.in(0)._1.flit
        in.ready := true.B
        assert(!in.valid)
      }
    }

    if (nocParams.hasCtrl) {
      ctrlNodes.zipWithIndex.map { case (c,i) =>
        if (c.isDefined) {
          io.router_ctrl(i) <> c.get.in(0)._1
        } else {
          io.router_ctrl(i) <> DontCare
        }
      }
    }

    // TODO: These assume a single clock-domain across the entire noc
    val debug_va_stall_ctr = RegInit(0.U(64.W))
    val debug_sa_stall_ctr = RegInit(0.U(64.W))
    val debug_any_stall_ctr = debug_va_stall_ctr + debug_sa_stall_ctr
    debug_va_stall_ctr := debug_va_stall_ctr + debugNodes.map(_.in(0)._1.va_stall.reduce(_+_)).reduce(_+_)
    debug_sa_stall_ctr := debug_sa_stall_ctr + debugNodes.map(_.in(0)._1.sa_stall.reduce(_+_)).reduce(_+_)

    dontTouch(debug_va_stall_ctr)
    dontTouch(debug_sa_stall_ctr)
    dontTouch(debug_any_stall_ctr)

    def prepend(s: String) = Seq(nocName, s).mkString(".")
    ElaborationArtefacts.add(prepend("noc.graphml"), graphML)

    val adjList = routers.map { r =>
      val outs = r.outParams.map(o => s"${o.destId}").mkString(" ")
      val egresses = r.egressParams.map(e => s"e${e.egressId}").mkString(" ")
      val ingresses = r.ingressParams.map(i => s"i${i.ingressId} ${r.nodeId}")
      (Seq(s"${r.nodeId} $outs $egresses") ++ ingresses).mkString("\n")
    }.mkString("\n")
    ElaborationArtefacts.add(prepend("noc.adjlist"), adjList)

    val xys = routers.map(r => {
      val n = r.nodeId
      val ids = (Seq(r.nodeId.toString)
        ++ r.egressParams.map(e => s"e${e.egressId}")
        ++ r.ingressParams.map(i => s"i${i.ingressId}")
      )
      val plotter = nocParams.topology.plotter
      val coords = (Seq(plotter.node(r.nodeId))
        ++ Seq.tabulate(r.egressParams.size ) { i => plotter. egress(i, r. egressParams.size, r.nodeId) }
        ++ Seq.tabulate(r.ingressParams.size) { i => plotter.ingress(i, r.ingressParams.size, r.nodeId) }
      )

      (ids zip coords).map { case (i, (x, y)) => s"$i $x $y" }.mkString("\n")
    }).mkString("\n")
    ElaborationArtefacts.add(prepend("noc.xy"), xys)

    val edgeProps = routers.map { r =>
      val outs = r.outParams.map { o =>
        (Seq(s"${r.nodeId} ${o.destId}") ++ (if (o.possibleFlows.size == 0) Some("unused") else None))
          .mkString(" ")
      }
      val egresses = r.egressParams.map { e =>
        (Seq(s"${r.nodeId} e${e.egressId}") ++ (if (e.possibleFlows.size == 0) Some("unused") else None))
          .mkString(" ")
      }
      val ingresses = r.ingressParams.map { i =>
        (Seq(s"i${i.ingressId} ${r.nodeId}") ++ (if (i.possibleFlows.size == 0) Some("unused") else None))
          .mkString(" ")
      }
      (outs ++ egresses ++ ingresses).mkString("\n")
    }.mkString("\n")
    ElaborationArtefacts.add(prepend("noc.edgeprops"), edgeProps)

    println(s"Constellation: $nocName Finished NoC RTL generation")
  }
}
