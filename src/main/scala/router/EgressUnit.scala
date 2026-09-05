package constellation.router

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.util._

import constellation.channel._
import constellation.routing.{FlowRoutingBundle}

class EgressUnit(coupleSAVA: Boolean, combineSAST: Boolean,
  inParams: Seq[ChannelParams], ingressParams: Seq[IngressChannelParams],
  cParam: EgressChannelParams, routingContexts: Seq[RouterRoutingContext])
  (implicit p: Parameters) extends AbstractOutputUnit(inParams, ingressParams, cParam)(p) {

  private val localShape = EgressUnitRoutingShape(
    cParam.nVirtualChannels, coupleSAVA, combineSAST)
  private val egressContexts = routingContexts.flatMap { context =>
    context.egressParams.zipWithIndex.collect {
      case (param, portId) if context.egressUnitRoutingShape(param) == localShape =>
        (context, portId, param)
    }
  }
  require(egressContexts.nonEmpty)
  private val portIdBits = log2Up(egressContexts.map(_._2).max + 1)

  class EgressUnitIO extends AbstractOutputUnitIO(inParams, ingressParams, cParam) {
    val node_id = Input(UInt(nodeIdBits.W))
    val port_id = Input(UInt(portIdBits.W))
    val out = Decoupled(new EgressFlit(
      EgressUnit.this.cParam.payloadBits,
      EgressUnit.this.cParam.ingressIdBits))
  }
  val io = IO(new EgressUnitIO)

  private def contextMatch(context: RouterRoutingContext, portId: Int): Bool =
    io.node_id === context.nodeId.U && io.port_id === portId.U

  val channel_empty = RegInit(true.B)
  val flow = Reg(new FlowRoutingBundle)
  val q = Module(new Queue(
    new EgressFlit(cParam.payloadBits, cParam.ingressIdBits),
    3 - (if (combineSAST) 1 else 0), flow=true))
  q.io.enq.valid := io.in(0).valid
  q.io.enq.bits.head := io.in(0).bits.head
  q.io.enq.bits.tail := io.in(0).bits.tail
  private val flowEntries = egressContexts.flatMap { case (context, portId, param) =>
    RouterRoutingContext.orderedFlows(param.possibleFlows).map { flow =>
      (context, portId, flow)
    }
  }
  if (flowEntries.isEmpty) {
    q.io.enq.bits.ingress_id := 0.U(1.W)
  } else {
    val matches = flowEntries.map { case (context, portId, flow) =>
      contextMatch(context, portId) &&
        flow.ingressNode.U === io.in(0).bits.flow.ingress_node &&
        flow.ingressNodeId.U === io.in(0).bits.flow.ingress_node_id
    }
    q.io.enq.bits.ingress_id := Mux1H(matches,
      flowEntries.map(_._3.ingressId.U(ingressIdBits.W)))
  }
  q.io.enq.bits.payload := io.in(0).bits.payload
  io.out <> q.io.deq
  assert(!(q.io.enq.valid && !q.io.enq.ready))

  io.credit_available(0) := q.io.count === 0.U
  io.channel_status(0).occupied := !channel_empty
  io.channel_status(0).flow := flow

  when (io.credit_alloc(0).alloc && io.credit_alloc(0).tail) {
    channel_empty := true.B
    if (coupleSAVA) io.channel_status(0).occupied := false.B
  }

  when (io.allocs(0).alloc) {
    channel_empty := false.B
    flow := io.allocs(0).flow
  }

}
