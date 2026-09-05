package constellation.channel

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.util._

import constellation.noc.{HasNoCParams}
import constellation.router.{ChannelHardwareShape, RouterRoutingContext}

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

  private def contextMatch(context: RouterRoutingContext, portId: Int): Bool =
    io.node_id === context.nodeId.U && io.port_id === portId.U

  val in_flight = RegInit(VecInit(Seq.fill(cParam.nVirtualChannels) { false.B }))
  for (i <- 0 until cParam.srcSpeedup) {
    val flit = io.in.flit(i)
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
        val allowed = monitorContexts.map { case (context, portId, channel) =>
          val flowSelected = channel.virtualChannelParams(vc).possibleFlows.toSeq
            .sortBy(f => (
              f.ingressId, f.egressId, f.vNetId, f.ingressNode,
              f.ingressNodeId, f.egressNode, f.egressNodeId, f.fifo))
            .map(_.isFlow(flit.bits.flow)).orR
          contextMatch(context, portId) && flowSelected
        }.orR
        assert(flit.bits.virt_channel_id =/= vc.U || allowed)
      }
    }
  }
}
