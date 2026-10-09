package constellation

import chisel3._
import chiseltest._
import constellation.routing._
import constellation.topology._
import org.scalatest.flatspec.AnyFlatSpec

private class MultiRucheMeshHardwareRoutingTester(
  topo: MultiRucheMesh2D,
  policies: Seq[RoutingRelation]) extends Module {
  private val layout = NodeIdLayout(topo)
  val io = IO(new Bundle {
    val source = Input(UInt(layout.width.W))
    val node = Input(UInt(layout.width.W))
    val next = Input(UInt(layout.width.W))
    val dest = Input(UInt(layout.width.W))
    val ingress = Input(Bool())
    val sourceVC = Input(UInt(2.W))
    val nextVC = Input(UInt(2.W))
    val allowed = Output(Vec(policies.size, Bool()))
  })
  private val source = HardwareRoutingChannel(io.source, io.node, io.sourceVC, 3, io.ingress)
  private val next = HardwareRoutingChannel(io.node, io.next, io.nextVC, 3, false.B)
  private val flow = HardwareRoutingFlow(0.U, io.source, io.dest)
  for ((policy, index) <- policies.zipWithIndex) {
    io.allowed(index) := policy.hardwareRouting.get(source, next, flow)
  }
}

class MultiRucheMeshHardwareRoutingTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MultiRucheMesh2D compact hardware routing"

  private val topologies = Seq(
    MultiRucheMesh2D(7, 5, 3, 1, 0, 2, 1, 4, 3),
    MultiRucheMesh2D(3, 2, 0),
    MultiRucheMesh2D(1, 5, 3, 0, 1, 0, 0, 0, 3),
    MultiRucheMesh2D(5, 1, 2, 1, 0, 3, 0),
    MultiRucheMesh2D(1, 1, 0))

  for (topo <- topologies) {
    it should s"match Scala legality for $topo" in {
      val policies = Seq(
        MultiRucheMesh2DDimensionOrderedRouting(0)(topo),
        MultiRucheMesh2DDimensionOrderedRouting(1)(topo),
        MultiRucheMesh2DMinimalRouting()(topo),
        MultiRucheMesh2DEscapeRouting(0, 1)(topo),
        MultiRucheMesh2DEscapeRouting(1, 2)(topo))
      assert(policies.forall(_.hardwareRouting.nonEmpty))
      val layout = NodeIdLayout(topo)
      if (topo.nX == 7) assert(layout.encode(7) == 8) // Dense row indices are not hardware IDs.
      val neighbors = (0 until topo.nNodes).map { node =>
        (0 until topo.nNodes).filter(next => topo.topo(node, next))
      }

      test(new MultiRucheMeshHardwareRoutingTester(topo, policies))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
          def check(src: Int, node: Int, next: Int, dest: Int,
            sourceVC: Int = 0, nextVC: Int = 2): Unit = {
            val ingressNode = if (src < 0) node else src
            dut.io.source.poke(layout.encode(ingressNode).U)
            dut.io.node.poke(layout.encode(node).U)
            dut.io.next.poke(layout.encode(next).U)
            dut.io.dest.poke(layout.encode(dest).U)
            dut.io.ingress.poke((src < 0).B)
            dut.io.sourceVC.poke(sourceVC.U)
            dut.io.nextVC.poke(nextVC.U)
            val source = ChannelRoutingInfo(src, node, sourceVC, 3)
            val out = ChannelRoutingInfo(node, next, nextVC, 3)
            val flow = FlowRoutingInfo(ingressNode, dest, 0, ingressNode, 0, dest, 0, fifo = false)
            for ((policy, index) <- policies.zipWithIndex) {
              dut.io.allowed(index).expect(policy.rel(source, out, flow).B,
                s"policy $index: $source -> $out, destination $dest")
            }
          }

          // Every physical output against every destination covers both signs,
          // all fitting-level fallbacks, no-overshoot rules, and both dimension orders.
          for {
            node <- 0 until topo.nNodes
            next <- neighbors(node) :+ node
            dest <- 0 until topo.nNodes
          } check(-1, node, next, dest)

          // Exercise adaptive -> escape, escape -> adaptive, and escape retention
          // with both one and two escape VCs, after regular and Ruche inputs.
          for {
            node <- 0 until topo.nNodes
            src <- neighbors(node)
            next <- neighbors(node)
            sourceVC <- 0 until 3
            nextVC <- 0 until 3
          } {
            check(src, node, next, next, sourceVC, nextVC)
            check(-1, node, next, next, sourceVC, nextVC)
          }
        }
    }
  }
}
