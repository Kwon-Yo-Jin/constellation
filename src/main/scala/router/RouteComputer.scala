package constellation.router

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{TruthTable, decoder}

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.DecodeLogic

import constellation.channel._
import constellation.routing.{FlowRoutingBundle, FlowRoutingInfo}
import constellation.noc.{HasNoCParams}

class RouteComputerReq(implicit val p: Parameters) extends Bundle with HasNoCParams {
  val src_virt_id = UInt(virtualChannelBits.W)
  val flow = new FlowRoutingBundle
}

class RouteComputerResp(
  val outParams: Seq[ChannelParams],
  val egressParams: Seq[EgressChannelParams])(implicit val p: Parameters) extends Bundle
    with HasRouterOutputParams {
  val vc_sel = MixedVec(allOutParams.map { u => Vec(u.nVirtualChannels, Bool()) })
}



class RouteComputer(
  val routingContexts: Seq[RouterRoutingContext],
  val inParams: Seq[ChannelParams],
  val outParams: Seq[ChannelParams],
  val ingressParams: Seq[IngressChannelParams],
  val egressParams: Seq[EgressChannelParams]
)(implicit val p: Parameters) extends Module
    with HasRouterInputParams
    with HasRouterOutputParams
    with HasNoCParams {
  val io = IO(new Bundle {
    val node_id = Input(UInt(nodeIdBits.W))
    val req = MixedVec(allInParams.map { u => Flipped(Decoupled(new RouteComputerReq)) })
    val resp = MixedVec(allInParams.map { u => Output(new RouteComputerResp(outParams, egressParams)) })
  })

  private val localContext = RouterRoutingContext(
    -1, inParams, outParams, ingressParams, egressParams)
  private val compatibleContexts = routingContexts.filter(_.hasSamePolicyShape(localContext))
  require(compatibleContexts.nonEmpty)

  (io.req zip io.resp).zipWithIndex.map { case ((req, resp), i) =>
    req.ready := true.B
    if (outParams.size == 0) {
      assert(!req.valid)
      resp.vc_sel := DontCare
    } else {

      val addr = req.bits.asUInt

      def toUInt(inputVc: Int, flow: FlowRoutingInfo): UInt = {
        val reqLiteral = (BigInt(inputVc) << req.bits.flow.getWidth) | flow.asLiteral(req.bits.flow)
        reqLiteral.U(addr.getWidth.W)
      }

      val width = outParams.map(_.nVirtualChannels).reduce(_+_)
      val decodedByContext = compatibleContexts.map { context =>
        val table = RouterRoutingContext.orderedFlows(
          context.allInParams(i).possibleFlows).flatMap { flow =>
          context.allInParams(i).channelRoutingInfos.map { input =>
            var row: String = "b"
            (0 until context.nOutputs).foreach { o =>
              (0 until context.outParams(o).nVirtualChannels).foreach { outVId =>
                val outputVC = context.outParams(o).virtualChannelParams(outVId)
                val active = outputVC.possibleFlows.contains(flow)
                row = row + (if (active && routingRelation(
                  input, context.outParams(o).channelRoutingInfos(outVId), flow)) "1" else "0")
              }
            }
            ((input.vc, flow), row)
          }
        }
        if (table.nonEmpty) {
          val truthTable = TruthTable(
            table.map { case ((inputVc, flow), row) =>
              (BitPat(toUInt(inputVc, flow)), BitPat(row))
            },
            BitPat("b" + "?" * width)
          )
          Reverse(decoder(addr, truthTable))
        } else {
          0.U(width.W)
        }
      }
      val decoded = PriorityMux(compatibleContexts.zip(decodedByContext).map {
        case (context, value) => (io.node_id === context.nodeId.U) -> value
      })
      var idx = 0

      (0 until nAllOutputs).foreach { o =>
        if (o < nOutputs) {
          (0 until outParams(o).nVirtualChannels).foreach { outVId =>
            resp.vc_sel(o)(outVId) := decoded(idx)
            idx += 1
          }
        } else {
          resp.vc_sel(o)(0) := false.B
        }
      }
    }
  }
}
