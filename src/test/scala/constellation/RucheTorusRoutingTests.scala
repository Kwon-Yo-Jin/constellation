package constellation

import constellation.channel._
import constellation.noc.{InternalNoCParams, NoCParams}
import constellation.routing._
import constellation.topology._
import org.scalatest.flatspec.AnyFlatSpec

class RucheTorusRoutingPolicyTest extends AnyFlatSpec {
  behavior of "Ruche torus routing"

  private def channel(src: Int, dst: Int, vc: Int = 1, nVC: Int = 2) =
    ChannelRoutingInfo(src, dst, vc, nVC)
  private def ingress(node: Int) = channel(-1, node, vc = 0, nVC = 1)
  private def flow(src: Int, dst: Int) =
    FlowRoutingInfo(src, dst, 0, src, 0, dst, 0, fifo = true)

  private def path(topo: PhysicalTopology, routing: RoutingRelation, src: Int, dst: Int): Seq[ChannelRoutingInfo] = {
    var channels = Seq(ingress(src))
    while (channels.last.dst != dst) {
      val current = channels.last
      val next = for {
        node <- 0 until topo.nNodes if topo.topo(current.dst, node)
        vc <- 0 until 2
        out = channel(current.dst, node, vc)
        if routing(current, out, flow(src, dst))
      } yield out
      assert(next.size == 1, s"Expected one next hop at $current for $src -> $dst: $next")
      assert(!channels.contains(next.head), s"Loop in $channels")
      channels :+= next.head
    }
    channels
  }

  private def validate(params: NoCParams): InternalNoCParams = {
    val output = new java.io.ByteArrayOutputStream
    try {
      // Runs Constellation's full connectivity and channel-dependency-cycle checks.
      Console.withOut(output) { InternalNoCParams(params) }
    } catch {
      case e: IllegalArgumentException => fail(s"${params.topology}: ${output.toString}\n${e.getMessage}", e)
    }
  }

  private def allToAll(topo: PhysicalTopology, routing: PhysicalTopology => RoutingRelation, nVC: Int) =
    NoCParams(
      topology = topo,
      channelParamGen = (_, _) => UserChannelParams(Seq.fill(nVC)(UserVirtualChannelParams(4))),
      ingresses = (0 until topo.nNodes).map(UserIngressParams(_)),
      egresses = (0 until topo.nNodes).map(UserEgressParams(_)),
      flows = Seq.tabulate(topo.nNodes, topo.nNodes)((s, d) => FlowParams(s, d, 0, fifo = true)).flatten,
      routingRelation = routing)

  it should "prefer fitting Ruche hops and finish X before Y" in {
    val uni = UnidirectionalRucheTorus2D(6, 6, 2, 2)
    val ur = DimensionOrderedUnidirectionalRucheTorus2DDatelineRouting()(uni)
    val up = path(uni, ur, 4, 14)
    assert(up.map(_.dst) == Seq(4, 0, 2, 14))
    assert(up.tail.map(_.vc) == Seq(0, 0, 1))
    assert(path(uni, ur, 0, 15).map(_.dst) == Seq(0, 2, 3, 15))

    val bi = BidirectionalRucheTorus2D(6, 6, 2, 2)
    val br = DimensionOrderedBidirectionalRucheTorus2DDatelineRouting()(bi)
    assert(path(bi, br, 1, 17).map(_.dst) == Seq(1, 5, 17))
    assert(path(bi, br, 1, 17).tail.map(_.vc) == Seq(0, 1))
    assert(path(bi, br, 1, 29).map(_.dst) == Seq(1, 5, 29))
    assert(path(bi, br, 0, 3).map(_.dst) == Seq(0, 2, 3)) // positive tie break
    assert(!br(ingress(0), channel(0, 12), flow(0, 14)))
    assert(!br(ingress(0), channel(0, 1), flow(0, 14)))
  }

  it should "mark skips crossing either dateline, including the first hop, and forbid recrossing" in {
    val uni = UnidirectionalRucheTorus2DDatelineRouting()(UnidirectionalRucheTorus2D(6, 6, 2, 2))
    val f = flow(2, 14)
    assert(uni(channel(2, 4), channel(4, 0, vc = 0), f))
    assert(!uni(channel(2, 4), channel(4, 0, vc = 1), f))
    assert(!uni(channel(2, 4, vc = 0), channel(4, 0, vc = 0), f))
    assert(uni(ingress(4), channel(4, 0, vc = 0), f))
    assert(!uni(ingress(4), channel(4, 0, vc = 1), f))
    assert(uni(channel(4, 0, vc = 0), channel(0, 2, vc = 0), f))
    assert(!uni(channel(4, 0, vc = 0), channel(0, 2, vc = 1), f))
    assert(uni(channel(4, 0, vc = 0), channel(0, 12, vc = 1), f)) // fresh Y phase

    val bi = BidirectionalRucheTorus2DDatelineRouting()(BidirectionalRucheTorus2D(6, 6, 2, 2))
    assert(bi(channel(3, 1), channel(1, 5, vc = 0), flow(3, 17)))
    assert(!bi(channel(3, 1), channel(1, 5, vc = 1), flow(3, 17)))
    assert(bi(ingress(1), channel(1, 5, vc = 0), flow(1, 17)))
    assert(!bi(channel(2, 3), channel(3, 1), flow(2, 1))) // no reversal
  }

  it should "keep regular-to-Ruche transitions legal and preserve priorities through both wrappers" in {
    val policies = Seq(
      UnidirectionalRucheTorus2DDatelineRouting()(UnidirectionalRucheTorus2D(6, 6, 2, 2)),
      BidirectionalRucheTorus2DDatelineRouting()(BidirectionalRucheTorus2D(6, 6, 2, 2)))
    for (routing <- policies) {
      assert(routing(channel(0, 1), channel(1, 3), flow(0, 5)))
      assert(routing.getNPrios(ingress(0)) == 2)
      assert(routing.getPrio(channel(0, 1), channel(1, 3), flow(0, 5)) == 0)
      assert(routing.getPrio(channel(0, 1), channel(1, 2), flow(0, 5)) == 1)
      assert(routing(ingress(0), channel(0, 12), flow(0, 14))) // no dimension ordering in base
    }
    val wrapped = NonblockingVirtualSubnetworksRouting(
      DimensionOrderedBidirectionalRucheTorus2DDatelineRouting(), 3, 2)(BidirectionalRucheTorus2D(6, 6, 2, 2))
    val f = flow(0, 14).copy(vNetId = 2)
    assert(wrapped(ingress(0), channel(0, 2, vc = 5, nVC = 7), f))
    assert(!wrapped(ingress(0), channel(0, 2, vc = 1, nVC = 7), f))
    assert(wrapped.getNPrios(ingress(0)) == 2)
    assert(wrapped.getPrio(ingress(0), channel(0, 2, vc = 5, nVC = 7), f) == 0)
    assert(wrapped.getPrio(ingress(0), channel(0, 1, vc = 5, nVC = 7), f) == 1)
  }

  it should "require at least two lowered VCs per network link" in {
    val topologies = Seq(
      UnidirectionalRucheTorus2D(3, 3, 2, 2) -> DimensionOrderedUnidirectionalRucheTorus2DDatelineRouting(),
      BidirectionalRucheTorus2D(5, 3, 2, 1) -> DimensionOrderedBidirectionalRucheTorus2DDatelineRouting())
    for ((topo, policy) <- topologies) {
      val error = intercept[IllegalArgumentException] {
        policy(topo)(ingress(0), channel(0, 2, vc = 0, nVC = 1), flow(0, 2))
      }
      assert(error.getMessage.contains("at least two virtual channels"))
    }
  }

  it should "pass all-to-all connectivity and full deadlock checks across Ruche factors" in {
    // Includes singleton axes, factor 0/1, half-size skips, complements of local
    // links, unequal dimensions, and extra nonzero VCs. No validation is skipped.
    for {
      (nX, nY) <- Seq((1, 1), (1, 5), (5, 1), (2, 2), (3, 4), (4, 5), (6, 6))
      rx <- 0 until nX
      ry <- 0 until nY
      (topo, policy) <- Seq(
        UnidirectionalRucheTorus2D(nX, nY, rx, ry) -> DimensionOrderedUnidirectionalRucheTorus2DDatelineRouting(),
        BidirectionalRucheTorus2D(nX, nY, rx, ry) -> DimensionOrderedBidirectionalRucheTorus2DDatelineRouting())
    } {
      validate(allToAll(topo, policy, nVC = 2))
    }
    validate(allToAll(UnidirectionalRucheTorus2D(6, 5, 4, 3),
      DimensionOrderedUnidirectionalRucheTorus2DDatelineRouting(), nVC = 4))
    validate(allToAll(BidirectionalRucheTorus2D(6, 5, 3, 4),
      DimensionOrderedBidirectionalRucheTorus2DDatelineRouting(), nVC = 4))
  }

  it should "pass terminal and virtual-subnetwork connectivity and deadlock checks" in {
    for ((topo, policy) <- Seq(
      UnidirectionalRucheTorus2D(3, 3, 2, 2) -> DimensionOrderedUnidirectionalRucheTorus2DDatelineRouting(),
      BidirectionalRucheTorus2D(4, 3, 2, 1) -> DimensionOrderedBidirectionalRucheTorus2DDatelineRouting())) {
      val params = allToAll(topo, policy, nVC = 7)
      val nodes = (0 until 3).flatMap(_ => 0 until topo.nNodes)
      validate(params.copy(
        topology = TerminalRouter(topo),
        ingresses = nodes.map(UserIngressParams(_)),
        egresses = nodes.map(UserEgressParams(_)),
        flows = (0 until 3).flatMap(v => Seq.tabulate(topo.nNodes, topo.nNodes) { (s, d) =>
          FlowParams(v * topo.nNodes + s, v * topo.nNodes + d, v, fifo = true)
        }.flatten),
        routingRelation = NonblockingVirtualSubnetworksRouting(TerminalRouterRouting(policy), 3, 2)))
    }
  }
}
