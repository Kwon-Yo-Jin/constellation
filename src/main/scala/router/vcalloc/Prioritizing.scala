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

      case class PrioHelper(prio: Int, outId: Int, outVId: Int, inId: Int, inVId: Int, flow: FlowRoutingInfo)

      val prioMaps = compatibleContexts.map { context =>
        val prioMap = (0 until context.allOutParams.size).flatMap { i =>
          (0 until context.allOutParams(i).nVirtualChannels).flatMap { j =>
            (0 until context.allInParams.size).flatMap { m =>
              (0 until context.allInParams(m).nVirtualChannels).flatMap { n =>
                val flows = context.allInParams(m) match {
                  case iP: ChannelParams => iP.virtualChannelParams(n).possibleFlows
                  case iP: IngressChannelParams => iP.possibleFlows
                }
                RouterRoutingContext.orderedFlows(flows).map { flow =>
                  val outputActive = context.allOutParams(i) match {
                    case channel: ChannelParams =>
                      channel.virtualChannelParams(j).possibleFlows.contains(flow)
                    case channel: EgressChannelParams =>
                      channel.possibleFlows.contains(flow)
                  }
                  val prio = if (!outputActive || i >= context.outParams.size) {
                    // Egresses have fixed priority 0.
                    0
                  } else {
                    routingRelation.getPrio(
                      context.allInParams(m).channelRoutingInfos(n),
                      context.allOutParams(i).channelRoutingInfos(j),
                      flow)
                  }
                  require(prio < nPrios && prio >= 0,
                    s"Invalid $prio not in [0, $nPrios) ${context.allInParams(m).channelRoutingInfos(n)} ${context.allOutParams(i).channelRoutingInfos(j)}")
                  Option.when(outputActive)(PrioHelper(prio, i, j, m, n, flow))
                }
              }.flatten
            }
          }
        }
        context -> prioMap
      }

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
        val decodedByContext = prioMaps.map { case (context, prioMap) =>
          val lookup = prioMap.filter(t => t.outId == i && t.outVId == j).map { e =>
            val ref = (((e.inVId << addr_bundle.id.getWidth) | e.inId) <<
              addr_bundle.flow.getWidth) | e.flow.asLiteral(flow)
            (BitPat(ref.U(addr_bundle.getWidth.W)), BitPat((1 << e.prio).U(nPrios.W)))
          }
          val decoded = if (lookup.isEmpty) {
            0.U(nPrios.W)
          } else if (lookup.map(_._2).distinct.size == 1) {
            BitPat.bitPatToUInt(lookup.head._2)
          } else {
            DecodeLogic(addr, BitPat.dontCare(nPrios), lookup)
          }
          (io.node_id === context.nodeId.U) -> decoded
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
