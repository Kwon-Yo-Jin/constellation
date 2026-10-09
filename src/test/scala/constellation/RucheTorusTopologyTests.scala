package constellation

import constellation.topology._
import org.scalatest.flatspec.AnyFlatSpec

class RucheTorusTopologyTest extends AnyFlatSpec {
  behavior of "Ruche torus topologies"

  private def destinations(topo: PhysicalTopology, src: Int): Set[Int] =
    (0 until topo.nNodes).filter(topo.topo(src, _)).toSet

  private def physicalChannels(topo: PhysicalTopology): Int =
    (0 until topo.nNodes).map(src =>
      (0 until topo.nNodes).map(topo.channelMultiplicity(src, _)).sum).sum

  it should "wrap regular and Ruche links only in positive directions for the unidirectional torus" in {
    val topo = UnidirectionalRucheTorus2D(7, 6, 2, 4)
    assert(topo.nNodes == 42)
    assert(destinations(topo, 0) == Set(1, 7, 2, 28))
    assert(destinations(topo, 41) == Set(35, 6, 36, 27))
    assert(!topo.topo(1, 0))
    assert(!topo.topo(2, 0))
    for (src <- 0 until topo.nNodes) {
      assert(destinations(topo, src).size == 4)
      assert(!topo.topo(src, src))
      for (dst <- destinations(topo, src)) {
        assert(src % topo.nX == dst % topo.nX || src / topo.nX == dst / topo.nX)
      }
    }
    assert(physicalChannels(topo) == 4 * topo.nNodes)
  }

  it should "wrap regular and Ruche links in both directions for the bidirectional torus" in {
    val topo = BidirectionalRucheTorus2D(7, 7, 2, 3)
    assert(destinations(topo, 0) == Set(1, 6, 7, 42, 2, 5, 21, 28))
    assert(destinations(topo, 48) == Set(42, 47, 6, 41, 43, 46, 20, 27))
    for (src <- 0 until topo.nNodes) {
      assert(destinations(topo, src).size == 8)
      assert(!topo.topo(src, src))
      for (dst <- destinations(topo, src)) {
        assert(topo.channelMultiplicity(src, dst) == topo.channelMultiplicity(dst, src))
        assert(src % topo.nX == dst % topo.nX || src / topo.nX == dst / topo.nX)
      }
    }
    assert(physicalChannels(topo) == 8 * topo.nNodes)
  }

  it should "reduce to the existing torus when skips are disabled, including singleton dimensions" in {
    for (nX <- 1 to 4; nY <- 1 to 4) {
      val pairs = Seq(
        UnidirectionalRucheTorus2D(nX, nY, 0, 0) -> UnidirectionalTorus2D(nX, nY),
        BidirectionalRucheTorus2D(nX, nY, 0, 0) -> BidirectionalTorus2D(nX, nY))
      for ((ruche, base) <- pairs; src <- 0 until base.nNodes; dst <- 0 until base.nNodes) {
        assert(ruche.topo(src, dst) == base.topo(src, dst))
        assert(ruche.channelMultiplicity(src, dst) == base.channelMultiplicity(src, dst))
      }
    }
    assert(destinations(UnidirectionalRucheTorus2D(6, 5, 0, 2), 0) == Set(1, 6, 12))
    assert(destinations(BidirectionalRucheTorus2D(6, 5, 2, 0), 0) == Set(1, 5, 6, 24, 2, 4))
  }

  it should "double physical channels at factor one, including wraparound links" in {
    for ((nX, nY) <- Seq((2, 2), (6, 5))) {
      val pairs = Seq(
        UnidirectionalRucheTorus2D(nX, nY, rucheFactor = 1) -> UnidirectionalTorus2D(nX, nY),
        BidirectionalRucheTorus2D(nX, nY, rucheFactor = 1) -> BidirectionalTorus2D(nX, nY))
      for ((ruche, base) <- pairs) {
        for (src <- 0 until base.nNodes; dst <- 0 until base.nNodes) {
          assert(ruche.channelMultiplicity(src, dst) == 2 * base.channelMultiplicity(src, dst))
        }
        assert(physicalChannels(ruche) == 2 * physicalChannels(base))
      }
    }
  }

  it should "handle coincident skip directions and skips parallel to local links" in {
    val bidirectional = BidirectionalRucheTorus2D(6, 6, 3, 5)
    assert(bidirectional.channelMultiplicity(0, 3) == 1)
    assert(bidirectional.channelMultiplicity(3, 0) == 1)
    assert(bidirectional.channelMultiplicity(0, 6) == 2)
    assert(bidirectional.channelMultiplicity(0, 30) == 2)
    assert(bidirectional.channelMultiplicity(0, 0) == 0)
    val unidirectional = UnidirectionalRucheTorus2D(6, 6, 3, 5)
    assert(unidirectional.channelMultiplicity(0, 3) == 1)
    assert(unidirectional.channelMultiplicity(3, 0) == 1)
    assert(unidirectional.channelMultiplicity(0, 6) == 1)
    assert(unidirectional.channelMultiplicity(0, 30) == 1)
  }

  it should "reject invalid dimensions and Ruche factors" in {
    val constructors: Seq[(Int, Int, Int, Int) => PhysicalTopology] = Seq(
      (x, y, rx, ry) => UnidirectionalRucheTorus2D(x, y, rx, ry),
      (x, y, rx, ry) => BidirectionalRucheTorus2D(x, y, rx, ry))
    val invalid = Seq((0, 6, 0, 0), (6, 0, 0, 0), (-1, 6, 0, 0), (6, -1, 0, 0),
      (6, 6, -1, 0), (6, 6, 0, -1), (6, 6, 6, 0), (6, 6, 0, 6),
      (6, 6, 7, 0), (6, 6, 0, 7), (1, 6, 1, 0), (6, 1, 0, 1))
    for (construct <- constructors; (x, y, rx, ry) <- invalid) {
      intercept[IllegalArgumentException] { construct(x, y, rx, ry) }
    }
    assert(UnidirectionalRucheTorus2D(6, 5, rucheFactor = 2) == UnidirectionalRucheTorus2D(6, 5, 2, 2))
    assert(BidirectionalRucheTorus2D(6, 5, rucheFactor = 2) == BidirectionalRucheTorus2D(6, 5, 2, 2))
  }

  it should "preserve grid node IDs and physical channel multiplicity through TerminalRouter" in {
    val topologies = Seq(
      UnidirectionalRucheTorus2D(6, 5, 1, 2),
      BidirectionalRucheTorus2D(6, 5, 1, 2))
    for (topo <- topologies) {
      val terminal = TerminalRouter(topo)
      assert(NodeIdLayout(topo) == GridNodeIdLayout(6, 5))
      assert(topo.plotter.node(29) == (5.0, 4.0))
      for (src <- 0 until topo.nNodes) {
        assert(terminal.channelMultiplicity(src, src + topo.nNodes) == 1)
        assert(terminal.channelMultiplicity(src + topo.nNodes, src) == 1)
        for (dst <- 0 until topo.nNodes) {
          assert(terminal.channelMultiplicity(src + topo.nNodes, dst + topo.nNodes) ==
            topo.channelMultiplicity(src, dst))
        }
      }
    }
  }
}
