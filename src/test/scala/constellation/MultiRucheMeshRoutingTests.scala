package constellation

import constellation.channel._
import constellation.noc.{InternalNoCParams, NoCParams}
import constellation.routing._
import constellation.topology._
import org.scalatest.flatspec.AnyFlatSpec

class MultiRucheMeshRoutingPolicyTest extends AnyFlatSpec {
  behavior of "Multi-level Ruche mesh routing"

  private def channel(src: Int, dst: Int, vc: Int = 0, nVC: Int = 1) =
    ChannelRoutingInfo(src, dst, vc, nVC)
  private def ingress(node: Int) = channel(-1, node)
  private def flow(src: Int, dst: Int) =
    FlowRoutingInfo(src, dst, 0, src, 0, dst, 0, fifo = true)

  private def path(topo: MultiRucheMesh2D, routing: RoutingRelation, src: Int, dst: Int): Seq[Int] = {
    var channels = Seq(ingress(src))
    while (channels.last.dst != dst) {
      val current = channels.last
      val next = (0 until topo.nNodes).filter(topo.topo(current.dst, _))
        .map(channel(current.dst, _)).filter(routing(current, _, flow(src, dst)))
      assert(next.size == 1, s"Expected one next hop at $current for $src -> $dst: $next")
      assert(!channels.contains(next.head), s"Loop in $channels")
      channels :+= next.head
    }
    channels.map(_.dst)
  }

  private def validate(params: NoCParams): InternalNoCParams = {
    val output = new java.io.ByteArrayOutputStream
    try {
      // Exercise production connectivity and escape/full-channel dependency checks.
      Console.withOut(output) { InternalNoCParams(params) }
    } catch {
      case e: IllegalArgumentException => fail(s"${params.topology}: ${output.toString}\n${e.getMessage}", e)
    }
  }

  private def allToAll(topo: PhysicalTopology, routing: PhysicalTopology => RoutingRelation,
    nVC: Int, fifo: Boolean = false) =
    NoCParams(
      topology = topo,
      channelParamGen = (_, _) => UserChannelParams(Seq.fill(nVC)(UserVirtualChannelParams(4))),
      ingresses = (0 until topo.nNodes).map(UserIngressParams(_)),
      egresses = (0 until topo.nNodes).map(UserEgressParams(_)),
      flows = Seq.tabulate(topo.nNodes, topo.nNodes)((s, d) => FlowParams(s, d, 0, fifo = fifo)).flatten,
      routingRelation = routing)

  it should "choose the longest fitting level in either sign and either dimension order" in {
    val topo = MultiRucheMesh2D(128, 128, 3, 2, 2, 6, 6, 9, 9)
    val xy = MultiRucheMesh2DDimensionOrderedRouting()(topo)
    val yx = MultiRucheMesh2DDimensionOrderedRouting(firstDim = 1)(topo)
    val dst = 17 + 17 * 128
    assert(path(topo, xy, 0, dst) == Seq(0, 9, 15, 17, 1169, 1937, 2193))
    assert(path(topo, xy, dst, 0) == Seq(2193, 2184, 2178, 2176, 1024, 256, 0))
    assert(path(topo, yx, 0, dst) == Seq(0, 1152, 1920, 2176, 2185, 2191, 2193))
    assert(path(topo, xy, 0, 16) == Seq(0, 9, 15, 16))
    assert(!xy(ingress(0), channel(0, 6), flow(0, dst)))
    assert(!xy(ingress(0), channel(0, 9 * 128), flow(0, dst)))
    assert(!xy(ingress(9), channel(9, 18), flow(9, 16)))
  }

  it should "keep every toward-destination physical channel available to minimal routing" in {
    val topo = MultiRucheMesh2D(128, 128, 3, 2, 2, 6, 6, 9, 9)
    val minimal = MultiRucheMesh2DMinimalRouting()(topo)
    val src = channel(0, 1)
    val f = flow(0, 17 + 17 * 128)
    for (step <- Seq(1, 2, 6, 9)) {
      assert(minimal(src, channel(1, 1 + step), f))
      assert(minimal(src, channel(1, 1 + 128 * step), f))
    }
    // An earlier regular hop does not lock the packet out of any Ruche level.
    assert(minimal(channel(9, 10), channel(10, 16), flow(0, 17)))
    assert(!minimal(channel(9, 10), channel(10, 19), flow(0, 17)))
    assert(!minimal(src, channel(1, 0), f))
    assert(minimal(channel(2193, 2192), channel(2192, 2183), flow(2193, 0)))
    assert(!minimal(ingress(0), channel(0, 128), flow(0, 17)))
  }

  it should "rank later Ruche levels above earlier levels and regular channels on both axes" in {
    val topo = MultiRucheMesh2D(128, 128, 3, 2, 2, 6, 6, 9, 9)
    for (routing <- Seq(MultiRucheMesh2DDimensionOrderedRouting()(topo),
      MultiRucheMesh2DMinimalRouting()(topo))) {
      assert(routing.getNPrios(ingress(0)) == 4)
      for ((step, priority) <- Seq(9 -> 0, 6 -> 1, 2 -> 2, 1 -> 3)) {
        assert(routing.getPrio(ingress(0), channel(0, step), flow(0, 17)) == priority)
        assert(routing.getPrio(ingress(0), channel(0, 128 * step), flow(0, 17 * 128)) == priority)
        assert(routing.getPrio(ingress(17), channel(17, 17 - step), flow(17, 0)) == priority)
        assert(routing.getPrio(ingress(17 * 128), channel(17 * 128, (17 - step) * 128),
          flow(17 * 128, 0)) == priority)
      }
    }
  }

  it should "validate increasing enabled factors independently for X and Y" in {
    val policies = Seq(MultiRucheMesh2DDimensionOrderedRouting(),
      MultiRucheMesh2DMinimalRouting(), MultiRucheMesh2DEscapeRouting())
    for (policy <- policies) {
      // X can exceed Y at every level; their order is not compared across axes.
      policy(MultiRucheMesh2D(8, 8, 2, 4, 1, 5, 2))
      policy(MultiRucheMesh2D(8, 8, 3, 1, 3, 0, 0, 4, 5))
      policy(MultiRucheMesh2D(8, 8, 3, 0, 1, 2, 0, 4, 3))
      for (topo <- Seq(
        MultiRucheMesh2D(8, 8, 2, 4, 1, 2, 3), // X decreases
        MultiRucheMesh2D(8, 8, 2, 1, 4, 3, 2), // Y decreases
        MultiRucheMesh2D(8, 8, 2, 2, 1, 2, 3), // X repeats
        MultiRucheMesh2D(8, 8, 2, 1, 2, 3, 2), // Y repeats
        MultiRucheMesh2D(8, 8, 3, 4, 1, 0, 2, 2, 3))) {
        intercept[IllegalArgumentException] { policy(topo) }
      }
    }
    val topo = MultiRucheMesh2D(6, 6, 2, 2, 2, 4, 4)
    for (firstDim <- Seq(-1, 2)) {
      intercept[IllegalArgumentException] { MultiRucheMesh2DDimensionOrderedRouting(firstDim)(topo) }
      intercept[IllegalArgumentException] { MultiRucheMesh2DEscapeRouting(firstDim)(topo) }
    }
    for (nEscape <- Seq(0, -1)) {
      intercept[IllegalArgumentException] { MultiRucheMesh2DEscapeRouting(nEscapeChannels = nEscape)(topo) }
    }
  }

  it should "handle zero levels, disabled axes, and factor-one parallel channels" in {
    val plain = MultiRucheMesh2D(4, 3, 0)
    val plainRouting = MultiRucheMesh2DDimensionOrderedRouting()(plain)
    assert(path(plain, plainRouting, 0, 11) == Seq(0, 1, 2, 3, 7, 11))
    assert(plainRouting.getNPrios(ingress(0)) == 1)
    assert(plainRouting.getPrio(ingress(0), channel(0, 1), flow(0, 3)) == 0)

    val disabled = MultiRucheMesh2D(6, 6, 3, 0, 1, 2, 0, 4, 3)
    val dor = MultiRucheMesh2DDimensionOrderedRouting()(disabled)
    assert(path(disabled, dor, 0, 35) == Seq(0, 4, 5, 23, 29, 35))
    val minimal = MultiRucheMesh2DMinimalRouting()(disabled)
    assert(minimal.getNPrios(ingress(0)) == 4)
    assert(minimal.getPrio(ingress(0), channel(0, 1), flow(0, 35)) == 3)
    assert(minimal.getPrio(ingress(0), channel(0, 6), flow(0, 35)) == 2)
    assert(minimal.getPrio(ingress(0), channel(0, 2), flow(0, 35)) == 1)
    assert(minimal.getPrio(ingress(0), channel(0, 18), flow(0, 35)) == 0)

    val parallel = MultiRucheMesh2D(6, 6, 2, 1, 1, 4, 4)
    val parallelRouting = MultiRucheMesh2DMinimalRouting()(parallel)
    assert(parallel.channelMultiplicity(0, 1) == 2)
    assert(parallelRouting.getPrio(ingress(0), channel(0, 1), flow(0, 5)) == 1)
  }

  it should "match single-level Ruche route legality" in {
    val multi = MultiRucheMesh2D(6, 5, 1, 2, 3)
    val single = RucheMesh2D(6, 5, 2, 3)
    val policies = Seq(
      MultiRucheMesh2DDimensionOrderedRouting()(multi) -> RucheMesh2DDimensionOrderedRouting()(single),
      MultiRucheMesh2DDimensionOrderedRouting(1)(multi) -> RucheMesh2DDimensionOrderedRouting(1)(single),
      MultiRucheMesh2DMinimalRouting()(multi) -> RucheMesh2DMinimalRouting()(single),
      MultiRucheMesh2DEscapeRouting()(multi) -> RucheMesh2DEscapeRouting()(single))
    for {
      (actual, expected) <- policies
      node <- 0 until multi.nNodes
      next <- 0 until multi.nNodes if multi.topo(node, next)
      dst <- 0 until multi.nNodes
      vc <- 0 until 2
    } {
      val out = channel(node, next, vc, nVC = 2)
      assert(actual(ingress(node), out, flow(node, dst)) == expected(ingress(node), out, flow(node, dst)))
    }
  }

  it should "allow adaptive-to-escape transitions but never leave the escape VCs" in {
    val topo = MultiRucheMesh2D(6, 6, 2, 2, 2, 4, 4)
    for (nEscape <- Seq(1, 2)) {
      val routing = MultiRucheMesh2DEscapeRouting(nEscapeChannels = nEscape)(topo)
      val nVC = nEscape + 1
      val adaptive = channel(0, 1, vc = nEscape, nVC = nVC)
      val f = flow(0, 35)
      for (escape <- 0 until nEscape) {
        assert(routing.isEscape(channel(1, 5, escape, nVC), 0))
        assert(routing(adaptive, channel(1, 5, escape, nVC), f))
        assert(routing(channel(0, 1, escape, nVC), channel(1, 5, escape, nVC), f))
        assert(!routing(channel(0, 1, escape, nVC), channel(1, 5, nEscape, nVC), f))
        assert(!routing(adaptive, channel(1, 3, escape, nVC), f))
        assert(!routing(adaptive, channel(1, 25, escape, nVC), f))
      }
      assert(!routing.isEscape(adaptive, 0))
      assert(routing.isEscape(ingress(0), 0))
      assert(routing(adaptive, channel(1, 3, nEscape, nVC), f))
      assert(routing(adaptive, channel(1, 25, nEscape, nVC), f))
      // Returning to X after an adaptive Y hop is a valid entry to X-first escape routing.
      assert(routing(channel(0, 6, nEscape, nVC), channel(6, 10, 0, nVC), f))
    }
  }

  it should "prioritize Ruche level before adaptive-versus-escape VC selection" in {
    val topo = MultiRucheMesh2D(12, 12, 3, 2, 2, 6, 6, 9, 9)
    val routing = MultiRucheMesh2DEscapeRouting()(topo)
    assert(routing.getNPrios(ingress(0)) == 8)
    for ((step, levelPriority) <- Seq(9 -> 0, 6 -> 1, 2 -> 2, 1 -> 3)) {
      assert(routing.getPrio(ingress(0), channel(0, step, 1, 2), flow(0, 11)) == 2 * levelPriority)
      assert(routing.getPrio(ingress(0), channel(0, step, 0, 2), flow(0, 11)) == 2 * levelPriority + 1)
    }
    assert(routing.getPrio(ingress(0), channel(0, 9, 0, 2), flow(0, 11)) <
      routing.getPrio(ingress(0), channel(0, 6, 1, 2), flow(0, 11)))
  }

  it should "preserve level priorities and escape classes through terminal and virtual-subnetwork wrappers" in {
    val topo = MultiRucheMesh2D(6, 6, 2, 2, 2, 4, 4)
    val wrapped = NonblockingVirtualSubnetworksRouting(
      TerminalRouterRouting(MultiRucheMesh2DEscapeRouting()), 3, 2)(TerminalRouter(topo))
    val offset = topo.nNodes
    val src = channel(offset, offset + 1, 5, 7)
    val largeEscape = channel(offset + 1, offset + 5, 4, 7)
    val largeAdaptive = largeEscape.copy(vc = 5)
    val smallAdaptive = channel(offset + 1, offset + 3, 5, 7)
    val f = flow(0, 35).copy(vNetId = 2)
    assert(wrapped(src, largeEscape, f))
    assert(wrapped(src, largeAdaptive, f))
    assert(!wrapped(src, largeAdaptive.copy(vc = 1), f))
    assert(wrapped.isEscape(largeEscape, 2))
    assert(!wrapped.isEscape(largeAdaptive, 2))
    assert(wrapped.getNPrios(src) == 6)
    assert(wrapped.getPrio(src, largeAdaptive, f) == 0)
    assert(wrapped.getPrio(src, largeEscape, f) == 1)
    assert(wrapped.getPrio(src, smallAdaptive, f) == 2)
  }

  it should "pass all-to-all connectivity and deadlock checks for both dimension orders and escape counts" in {
    val topologies = Seq(
      MultiRucheMesh2D(1, 1, 0),
      MultiRucheMesh2D(1, 5, 2, 0, 1, 0, 3),
      MultiRucheMesh2D(5, 1, 2, 1, 0, 3, 0),
      MultiRucheMesh2D(3, 3, 0),
      MultiRucheMesh2D(3, 3, 2, 1, 1, 2, 2),
      MultiRucheMesh2D(5, 4, 3, 0, 1, 2, 0, 4, 3),
      MultiRucheMesh2D(6, 6, 2, 2, 2, 4, 4))
    for (topo <- topologies; firstDim <- 0 until 2) {
      validate(allToAll(topo, MultiRucheMesh2DDimensionOrderedRouting(firstDim), nVC = 1, fifo = true))
      validate(allToAll(topo, MultiRucheMesh2DDimensionOrderedRouting(firstDim), nVC = 2, fifo = true))
      for (nEscape <- Seq(1, 2)) {
        validate(allToAll(topo, MultiRucheMesh2DEscapeRouting(firstDim, nEscape), nVC = nEscape + 1))
      }
    }
  }

  it should "reject FIFO flows when adaptive escape routing permits multiple next-nodes" in {
    val topo = MultiRucheMesh2D(3, 3, 1, 2, 2)
    val params = allToAll(topo, MultiRucheMesh2DEscapeRouting(), nVC = 2, fifo = true)
    val output = new java.io.ByteArrayOutputStream
    val error = intercept[IllegalArgumentException] {
      Console.withOut(output) { InternalNoCParams(params) }
    }
    assert(error.getMessage.contains("FIFO flow"))
    assert(error.getMessage.contains("multiple possible next-nodes"))
  }

  it should "pass production validation with terminal routers and nonblocking virtual subnetworks" in {
    val topo = MultiRucheMesh2D(4, 3, 2, 1, 1, 3, 2)
    for ((policy, fifo) <- Seq(MultiRucheMesh2DDimensionOrderedRouting() -> true,
      MultiRucheMesh2DDimensionOrderedRouting(1) -> true,
      MultiRucheMesh2DEscapeRouting() -> false, MultiRucheMesh2DEscapeRouting(1) -> false)) {
      val params = allToAll(topo, policy, nVC = 7, fifo = fifo)
      val nodes = (0 until 3).flatMap(_ => 0 until topo.nNodes)
      validate(params.copy(
        topology = TerminalRouter(topo),
        ingresses = nodes.map(UserIngressParams(_)),
        egresses = nodes.map(UserEgressParams(_)),
        flows = (0 until 3).flatMap(v => Seq.tabulate(topo.nNodes, topo.nNodes) { (s, d) =>
          FlowParams(v * topo.nNodes + s, v * topo.nNodes + d, v, fifo = fifo)
        }.flatten),
        routingRelation = NonblockingVirtualSubnetworksRouting(TerminalRouterRouting(policy), 3, 2)))
    }
  }
}
