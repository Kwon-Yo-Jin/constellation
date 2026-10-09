package constellation.channel

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.util._

import constellation.noc.{HasNoCParams}
import constellation.router.{ChannelHardwareShape, RouterRoutingContext}
import constellation.routing.FlowRoutingInfo

class NoCMonitor(
  val cParam: ChannelParams,
  routingContexts: Seq[RouterRoutingContext]
)(implicit val p: Parameters) extends Module with HasNoCParams {
  private val localVirtualChannels = cParam.nVirtualChannels
  private val monitorContexts = routingContexts.flatMap { context =>
    context.inParams.zipWithIndex.collect {
      case (param, portId) if param.nVirtualChannels == localVirtualChannels =>
        (context, portId, param)
    }
  }
  require(monitorContexts.nonEmpty)
  private val portIdBits = log2Up(monitorContexts.map(_._2).max + 1)

  val io = IO(new Bundle {
    val node_id = Input(UInt(nodeIdBits.W))
    val port_id = Input(UInt(portIdBits.W))
    val in = Input(new Channel(cParam))
  })

  dontTouch(io.node_id)
  dontTouch(io.port_id)

  private val contextMatches = monitorContexts.map { case (context, portId, _) =>
    io.node_id === runtimeNodeId(context.nodeId).U && io.port_id === portId.U
  }

  val in_flight = RegInit(VecInit(Seq.fill(cParam.nVirtualChannels) { false.B }))
  for (i <- 0 until cParam.srcSpeedup) {
    val flit = io.in.flit(i)
    // Build shared predicates outside the assertion's conditional scope. A flow
    // and its legal-flow set recur across many routing contexts and VCs.
    val flowMatches = scala.collection.mutable.Map.empty[FlowRoutingInfo, Bool]
    val flowSetMatches = scala.collection.mutable.Map.empty[Set[FlowRoutingInfo], Bool]
    val allowedMatches = scala.collection.mutable.Map.empty[Seq[Set[FlowRoutingInfo]], Bool]
    val allowedByVC = (0 until cParam.nVirtualChannels).map { vc =>
      val flowSets = monitorContexts.map(_._3.virtualChannelParams(vc).possibleFlows)
      allowedMatches.getOrElseUpdate(flowSets,
        contextMatches.zip(flowSets).map { case (active, flows) =>
          val flowSelected = flowSetMatches.getOrElseUpdate(flows,
            RouterRoutingContext.orderedFlows(flows).map { flow =>
              flowMatches.getOrElseUpdate(flow, flow.isFlow(flit.bits.flow))
            }.orR)
          active && flowSelected
        }.orR)
    }
    when (flit.valid) {
      when (flit.bits.head) {
        in_flight(flit.bits.virt_channel_id) := true.B
        assert (!in_flight(flit.bits.virt_channel_id), "Flit head/tail sequencing is broken")
      }
      when (flit.bits.tail) {
        in_flight(flit.bits.virt_channel_id) := false.B
      }
    }
    when (flit.valid && flit.bits.head) {
      for (vc <- 0 until cParam.nVirtualChannels) {
        assert(flit.bits.virt_channel_id =/= vc.U || allowedByVC(vc))
      }
    }
  }
}
