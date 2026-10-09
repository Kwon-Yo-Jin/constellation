package constellation

import constellation.topology._
import org.scalatest.flatspec.AnyFlatSpec

class MultiRucheMeshTopologyTest extends AnyFlatSpec {
  behavior of "MultiRucheMesh2D"

  private def destinations(topo: PhysicalTopology, src: Int): Set[Int] =
    (0 until topo.nNodes).filter(topo.topo(src, _)).toSet

  private def physicalChannels(topo: PhysicalTopology): Int =
    (0 until topo.nNodes).map(src =>
      (0 until topo.nNodes).map(topo.channelMultiplicity(src, _)).sum).sum

  private def assertEquivalent(actual: PhysicalTopology, expected: PhysicalTopology): Unit = {
    assert(actual.nNodes == expected.nNodes)
    for (src <- 0 until expected.nNodes; dst <- 0 until expected.nNodes) {
      assert(actual.topo(src, dst) == expected.topo(src, dst), s"connectivity $src -> $dst")
      assert(actual.channelMultiplicity(src, dst) == expected.channelMultiplicity(src, dst),
        s"physical channels $src -> $dst")
    }
  }

  it should "match the requested single-level Ruche mesh" in {
    val topo = MultiRucheMesh2D(6, 6, 1, 2, 2)
    assertEquivalent(topo, RucheMesh2D(6, 6, 2, 2))
    assert(topo.nLevels == 1)
    assert(topo.xRucheFactors == Seq(2))
    assert(topo.yRucheFactors == Seq(2))
    assert(destinations(topo, 0) == Set(1, 2, 6, 12))
    assert(physicalChannels(topo) == 216)
  }

  it should "combine the requested two Ruche levels without duplicating local mesh channels" in {
    val topo = MultiRucheMesh2D(6, 6, 2, 2, 2, 4, 4)
    val mesh = Mesh2D(6, 6)
    val first = RucheMesh2D(6, 6, 2, 2)
    val second = RucheMesh2D(6, 6, 4, 4)
    assert(topo.xRucheFactors == Seq(2, 4))
    assert(topo.yRucheFactors == Seq(2, 4))
    assert(destinations(topo, 0) == Set(1, 2, 4, 6, 12, 24))
    for (src <- 0 until topo.nNodes; dst <- 0 until topo.nNodes) {
      assert(topo.channelMultiplicity(src, dst) == first.channelMultiplicity(src, dst) +
        second.channelMultiplicity(src, dst) - mesh.channelMultiplicity(src, dst))
      assert(topo.topo(src, dst) == (first.topo(src, dst) || second.topo(src, dst)))
    }
    assert(topo.channelMultiplicity(0, 1) == 1)
    assert(physicalChannels(topo) == 264)
  }

  it should "match every valid single-level factor combination on small meshes" in {
    for (nX <- 1 to 5; nY <- 1 to 5; rx <- 0 until nX; ry <- 0 until nY) {
      withClue(s"dimensions ($nX, $nY), factors ($rx, $ry): ") {
        assertEquivalent(MultiRucheMesh2D(nX, nY, 1, rx, ry), RucheMesh2D(nX, nY, rx, ry))
      }
    }
  }

  it should "reduce to the ordinary mesh for zero levels or disabled factors" in {
    for (nX <- 1 to 5; nY <- 1 to 5) {
      val zero = MultiRucheMesh2D(nX, nY, 0)
      assert(zero.xRucheFactors.isEmpty && zero.yRucheFactors.isEmpty)
      assertEquivalent(zero, Mesh2D(nX, nY))
      assertEquivalent(MultiRucheMesh2D(nX, nY, 3, 0, 0, 0, 0, 0, 0), Mesh2D(nX, nY))
    }
  }

  it should "configure any number of levels from factor pairs with independently enabled axes" in {
    val factors = Seq(2, 0, 0, 3, 4, 1, 2, 0)
    val topo = MultiRucheMesh2D(6, 6, factors.size / 2, factors: _*)
    assert(topo.nLevels == 4)
    assert(topo.rucheFactors == factors)
    assert(topo.xRucheFactors == Seq(2, 0, 4, 2))
    assert(topo.yRucheFactors == Seq(0, 3, 1, 0))
    assert(destinations(topo, 0) == Set(1, 2, 4, 6, 18))
    assert(topo.channelMultiplicity(0, 2) == 2)
    assert(topo.channelMultiplicity(0, 6) == 2)
    assert(topo.channelMultiplicity(0, 12) == 0)
    assert(topo.channelMultiplicity(0, 24) == 0)

    val layers = factors.grouped(2).map(pair => RucheMesh2D(6, 6, pair(0), pair(1))).toSeq
    val mesh = Mesh2D(6, 6)
    for (src <- 0 until topo.nNodes; dst <- 0 until topo.nNodes) {
      assert(topo.channelMultiplicity(src, dst) ==
        layers.map(_.channelMultiplicity(src, dst)).sum -
          (topo.nLevels - 1) * mesh.channelMultiplicity(src, dst))
    }
  }

  it should "preserve parallel channels for repeated factors and factor one at every level" in {
    val topo = MultiRucheMesh2D(6, 5, 3, 1, 2, 1, 2, 2, 0)
    assert(topo.channelMultiplicity(0, 1) == 3)
    assert(topo.channelMultiplicity(0, 2) == 1)
    assert(topo.channelMultiplicity(0, 6) == 1)
    assert(topo.channelMultiplicity(0, 12) == 2)
    assert(physicalChannels(topo) == 310)

    val mesh = Mesh2D(6, 5)
    val doubled = MultiRucheMesh2D(6, 5, 1, 1, 1)
    val tripled = MultiRucheMesh2D(6, 5, 2, 1, 1, 1, 1)
    for (src <- 0 until mesh.nNodes; dst <- 0 until mesh.nNodes) {
      assert(doubled.channelMultiplicity(src, dst) == 2 * mesh.channelMultiplicity(src, dst))
      assert(tripled.channelMultiplicity(src, dst) == 3 * mesh.channelMultiplicity(src, dst))
    }
  }

  it should "keep every channel bidirectional and axis-aligned without wrapping or self-links" in {
    val topo = MultiRucheMesh2D(6, 6, 2, 2, 2, 4, 4)
    assert(!topo.topo(0, 5))
    assert(!topo.topo(0, 30))
    assert(!topo.topo(5, 6))
    assert(!topo.topo(29, 30))
    for (src <- 0 until topo.nNodes) {
      assert(!topo.topo(src, src))
      assert(topo.channelMultiplicity(src, src) == 0)
      for (dst <- 0 until topo.nNodes) {
        assert(topo.channelMultiplicity(src, dst) == topo.channelMultiplicity(dst, src))
        assert(topo.topo(src, dst) == (topo.channelMultiplicity(src, dst) > 0))
        if (topo.topo(src, dst)) {
          assert(src % topo.nX == dst % topo.nX || src / topo.nX == dst / topo.nX)
        }
      }
    }
  }

  it should "reject invalid dimensions, level counts, factor counts, and factors" in {
    val invalid = Seq(
      (0, 6, 0, Seq.empty[Int]), (6, 0, 0, Seq.empty[Int]),
      (-1, 6, 0, Seq.empty[Int]), (6, -1, 0, Seq.empty[Int]),
      (6, 6, -1, Seq.empty[Int]), (6, 6, Int.MaxValue, Seq.empty[Int]),
      (6, 6, 0, Seq(0, 0)), (6, 6, 1, Seq.empty[Int]),
      (6, 6, 1, Seq(2)), (6, 6, 1, Seq(2, 2, 4)),
      (6, 6, 1, Seq(2, 2, 4, 4)), (6, 6, 2, Seq(2, 2)),
      (6, 6, 2, Seq(2, 2, -1, 0)), (6, 6, 2, Seq(2, 2, 0, -1)),
      (6, 6, 2, Seq(2, 2, 6, 0)), (6, 6, 2, Seq(2, 2, 0, 6)),
      (6, 6, 2, Seq(2, 2, 7, 0)), (6, 6, 2, Seq(2, 2, 0, 7)),
      (1, 6, 1, Seq(1, 0)), (6, 1, 1, Seq(0, 1)))
    for ((nX, nY, levels, factors) <- invalid) {
      withClue(s"dimensions ($nX, $nY), levels $levels, factors $factors: ") {
        intercept[IllegalArgumentException] {
          MultiRucheMesh2D(nX, nY, levels, factors: _*)
        }
      }
    }
  }

  it should "preserve grid coordinates and physical channel multiplicity through TerminalRouter" in {
    val topo = MultiRucheMesh2D(6, 5, 3, 1, 2, 1, 2, 4, 0)
    val terminal = TerminalRouter(topo)
    assert(NodeIdLayout(topo) == GridNodeIdLayout(6, 5))
    assert(topo.plotter.node(29) == (5.0, 4.0))
    assert(terminal.nNodes == 2 * topo.nNodes)
    for (src <- 0 until topo.nNodes; dst <- 0 until topo.nNodes) {
      assert(terminal.channelMultiplicity(src + topo.nNodes, dst + topo.nNodes) ==
        topo.channelMultiplicity(src, dst))
      assert(terminal.topo(src + topo.nNodes, dst + topo.nNodes) == topo.topo(src, dst))
      assert(terminal.channelMultiplicity(src, dst) == 0)
      assert(terminal.channelMultiplicity(src, dst + topo.nNodes) == (if (src == dst) 1 else 0))
      assert(terminal.channelMultiplicity(src + topo.nNodes, dst) == (if (src == dst) 1 else 0))
    }
  }
}
