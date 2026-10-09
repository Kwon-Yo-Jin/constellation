package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.random.{LFSR}

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.rocket.{DecodeLogic}
import freechips.rocketchip.util._

import constellation.channel._
import constellation.routing._

trait Prioritizing { this: VCAllocator =>
  def prioritizing(
    in: MixedVec[Vec[Bool]],
    inId: UInt,
    inVId: UInt,
    dests: Seq[ChannelRoutingInfo],
    flow: FlowRoutingBundle,
    fire: Bool): MixedVec[Vec[Bool]] = {
    val w = in.getWidth
    if (w > 1) {
      val localContext = RouterRoutingContext(-1, inParams, outParams, ingressParams, egressParams)
      val compatibleContexts = routingContexts.filter(_.hasSamePolicyShape(localContext))
      require(compatibleContexts.nonEmpty)
      val nPrios = compatibleContexts.flatMap(c => c.allOutParams ++ c.allInParams)
        .flatMap(_.channelRoutingInfos).map(c => routingRelation.getNPrios(c)).max

      class LookupBundle extends Bundle {
        val vid = UInt((inVId.getWidth max 1).W)
        val id = UInt((inId.getWidth max 1).W)
        val flow = new FlowRoutingBundle
      }

      val addr_bundle = Wire(new LookupBundle)
      val addr = addr_bundle.asUInt
      addr_bundle.vid := inVId
      addr_bundle.id := inId
      addr_bundle.flow := flow

      val in_prio = (0 until allOutParams.size).map { i => (0 until allOutParams(i).nVirtualChannels).map { j =>
        val decodedByContext = compatibleContexts.map { context =>
          val output = context.allOutParams(i)
          val next = output.channelRoutingInfos(j)
          val outputFlows = output match {
            case channel: ChannelParams => channel.virtualChannelParams(j).possibleFlows
            case channel: EgressChannelParams => channel.possibleFlows
          }

          def foreachPriority(ordered: Boolean)(visit: (Int, Int, FlowRoutingInfo, Int) => Unit): Unit = {
            context.allInParams.zipWithIndex.foreach { case (input, m) =>
              (0 until input.nVirtualChannels).foreach { n =>
                val flows = input match {
                  case channel: ChannelParams => channel.virtualChannelParams(n).possibleFlows
                  case channel: IngressChannelParams => channel.possibleFlows
                }
                val source = input.channelRoutingInfos(n)
                val candidates = if (ordered) RouterRoutingContext.orderedFlows(flows) else flows
                candidates.foreach { candidate =>
                  val outputActive = outputFlows.contains(candidate)
                  val prio = if (!outputActive || i >= context.outParams.size) {
                    // Egresses have fixed priority 0.
                    0
                  } else {
                    routingRelation.getPrio(source, next, candidate)
                  }
                  require(prio < nPrios && prio >= 0,
                    s"Invalid $prio not in [0, $nPrios) $source $next")
                  if (outputActive) visit(m, n, candidate, prio)
                }
              }
            }
          }

          // Most routing policies assign one priority to an entire output VC.
          // Check this without retaining the input-VC/flow Cartesian product.
          var firstPrio = -1
          var mixedPriorities = false
          foreachPriority(ordered = false) { (_, _, _, prio) =>
            if (firstPrio < 0) firstPrio = prio
            else if (prio != firstPrio) mixedPriorities = true
          }
          val decoded = if (firstPrio < 0) {
            0.U(nPrios.W)
          } else if (!mixedPriorities) {
            (1 << firstPrio).U(nPrios.W)
          } else {
            val lookup = Vector.newBuilder[(BitPat, BitPat)]
            foreachPriority(ordered = true) { (inId, inVId, candidate, prio) =>
              val ref = (((inVId << addr_bundle.id.getWidth) | inId) <<
                addr_bundle.flow.getWidth) | candidate.asLiteral(flow)
              lookup += ((BitPat(ref.U(addr_bundle.getWidth.W)), BitPat((1 << prio).U(nPrios.W))))
            }
            DecodeLogic(addr, BitPat.dontCare(nPrios), lookup.result())
          }
          (io.node_id === runtimeNodeId(context.nodeId).U) -> decoded
        }
        Mux(in(i)(j), PriorityMux(decodedByContext), 0.U(nPrios.W))
      }}

      val mask = RegInit(0.U(w.W))
      val prio_sels = (0 until nPrios).map { p =>
        val sel = Wire(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool())}))
        (0 until allOutParams.size).map { i => (0 until allOutParams(i).nVirtualChannels).map { j =>
           sel(i)(j) := in_prio(i)(j)(p)
        }}
        val full = Cat(sel.asUInt, sel.asUInt & ~mask)
        val oh = PriorityEncoderOH(full)
        (oh(w-1,0) | (oh >> w))
      }
      val prio_oh = (0 until nPrios).map { p =>
        in_prio.map(_.map(_(p)).orR).orR
      }
      val lowest_prio = PriorityEncoderOH(prio_oh)
      val sel = Mux1H(lowest_prio, prio_sels)


      when (fire) {
        mask := MuxCase(0.U, (0 until w).map { i =>
          sel(i) -> ~(0.U((i+1).W))
        })
      }
      sel.asTypeOf(MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) }))
    } else {
      in
    }
  }

  def inputAllocPolicy(flow: FlowRoutingBundle, vc_sel: MixedVec[Vec[Bool]], inId: UInt, inVId: UInt, fire: Bool) = {
    prioritizing(
      vc_sel,
      inId,
      inVId,
      allOutParams.map(_.channelRoutingInfos).flatten,
      flow,
      fire)
  }

  def outputAllocPolicy(channel: ChannelRoutingInfo, flows: Seq[FlowRoutingBundle], reqs: Seq[Bool], fire: Bool): Vec[Bool] = {
    require(false, "Not supported")
    VecInit(reqs)
  }
}

class PrioritizingSingleVCAllocator(vP: VCAllocatorParams)(implicit p: Parameters) extends SingleVCAllocator(vP)(p)
    with Prioritizing
