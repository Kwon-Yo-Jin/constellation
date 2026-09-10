package constellation.topology

/** A collision-free packed representation of a topology node's coordinates.
  *
  * Topology indices remain the dense Int keys used by the Scala graph.  This
  * layout is used only at the hardware boundary, where structured coordinates
  * make regular routing policies substantially cheaper than flat lookup tables.
  */
sealed trait NodeIdLayout {
  def width: Int
  def encode(node: Int): BigInt
  def parts(node: Int): Seq[BigInt]
  def partWidths: Seq[Int]

  final def allIds: IndexedSeq[BigInt] =
    (0 until nNodes).map(encode)
  def nNodes: Int

  protected final def requireNode(node: Int): Unit =
    require(node >= 0 && node < nNodes, s"node $node is outside [0, $nNodes)")
}

object NodeIdLayout {
  def bitsFor(count: Int): Int = math.max(1, BigInt(math.max(count - 1, 0)).bitLength)

  def apply(topo: PhysicalTopology): NodeIdLayout = {
    val layout: NodeIdLayout = topo match {
      case mesh: Mesh2DLikePhysicalTopology => GridNodeIdLayout(mesh.nX, mesh.nY)
      case butterfly: Butterfly => ButterflyNodeIdLayout(butterfly.nFly, butterfly.height)
      case terminal: TerminalRouter => TerminalNodeIdLayout(apply(terminal.base))
      case hierarchy: HierarchicalTopology =>
        HierarchicalNodeIdLayout(
          apply(hierarchy.base),
          hierarchy.children.map(c => apply(c.topo)),
          hierarchy.children.map(_.src),
          hierarchy.offsets,
          hierarchy.nNodes)
      case _ => FlatNodeIdLayout(topo.nNodes)
    }
    require(layout.allIds.distinct.size == topo.nNodes,
      s"runtime node IDs are not unique for $topo: ${layout.allIds}")
    require(layout.allIds.forall(_.bitLength <= layout.width),
      s"runtime node ID exceeds ${layout.width} bits for $topo")
    layout
  }
}

/** One-dimensional topologies and arbitrary graphs retain one local part. */
case class FlatNodeIdLayout(nNodes: Int) extends NodeIdLayout {
  require(nNodes > 0)
  val width = NodeIdLayout.bitsFor(nNodes)
  val partWidths = Seq(width)
  def encode(node: Int): BigInt = { requireNode(node); BigInt(node) }
  def parts(node: Int): Seq[BigInt] = Seq(encode(node))
}

/** A two-dimensional coordinate. X occupies the low bits and Y the high bits. */
case class GridNodeIdLayout(nX: Int, nY: Int) extends NodeIdLayout {
  require(nX > 0 && nY > 0)
  val nNodes = nX * nY
  val xWidth = NodeIdLayout.bitsFor(nX)
  val yWidth = NodeIdLayout.bitsFor(nY)
  val width = xWidth + yWidth
  val partWidths = Seq(xWidth, yWidth)
  def parts(node: Int): Seq[BigInt] = {
    requireNode(node)
    Seq(BigInt(node % nX), BigInt(node / nX))
  }
  def encode(node: Int): BigInt = {
    val Seq(x, y) = parts(node)
    (y << xWidth) | x
  }
}

/** Butterfly coordinates are the stage and the row within that stage. */
case class ButterflyNodeIdLayout(nStages: Int, height: Int) extends NodeIdLayout {
  require(nStages > 0 && height > 0)
  val nNodes = nStages * height
  val stageWidth = NodeIdLayout.bitsFor(nStages)
  val rowWidth = NodeIdLayout.bitsFor(height)
  val width = stageWidth + rowWidth
  val partWidths = Seq(stageWidth, rowWidth)
  def parts(node: Int): Seq[BigInt] = {
    requireNode(node)
    Seq(BigInt(node / height), BigInt(node % height))
  }
  def encode(node: Int): BigInt = {
    val Seq(stage, row) = parts(node)
    (row << stageWidth) | stage
  }
}

/** TerminalRouter adds one role bit without destroying the base coordinates. */
case class TerminalNodeIdLayout(base: NodeIdLayout) extends NodeIdLayout {
  val nNodes = 2 * base.nNodes
  val width = base.width + 1
  val partWidths = base.partWidths :+ 1
  def parts(node: Int): Seq[BigInt] = {
    requireNode(node)
    val baseNode = node % base.nNodes
    base.parts(baseNode) :+ BigInt(if (node >= base.nNodes) 1 else 0)
  }
  def encode(node: Int): BigInt = {
    requireNode(node)
    val baseNode = node % base.nNodes
    (base.encode(baseNode) << 1) | (if (node >= base.nNodes) 1 else 0)
  }
}

/**
  * A hierarchy appends one child-level part to the base prefix.
  *
  * The child part is {valid, attachment, child-local-ID}.  Base routers all
  * use valid=0, so their complete child part is the same ROOT value.  A valid
  * bit prevents child node zero from aliasing the base router, and the
  * attachment field keeps multiple children on one base router unique.
  */
case class HierarchicalNodeIdLayout(
  base: NodeIdLayout,
  children: Seq[NodeIdLayout],
  attachmentBaseNodes: Seq[Int],
  offsets: Seq[Int],
  nNodes: Int) extends NodeIdLayout {
  require(children.size == attachmentBaseNodes.size)
  require(offsets.size == children.size + 1)
  val maxChildWidth = children.map(_.width).foldLeft(1)(math.max)
  val attachmentWidth = NodeIdLayout.bitsFor(math.max(children.size, 1))
  val childPartWidth = 1 + attachmentWidth + maxChildWidth
  val width = base.width + childPartWidth
  val partWidths = base.partWidths :+ childPartWidth

  private def childIndex(node: Int): Int =
    offsets.drop(1).indexWhere(_ > node)

  def parts(node: Int): Seq[BigInt] = {
    requireNode(node)
    if (node < base.nNodes) {
      base.parts(node) :+ BigInt(0)
    } else {
      val child = childIndex(node)
      require(child >= 0)
      val local = node - offsets(child)
      val childPart =
        (BigInt(1) << (attachmentWidth + maxChildWidth)) |
        (BigInt(child) << maxChildWidth) |
        children(child).encode(local)
      base.parts(attachmentBaseNodes(child)) :+ childPart
    }
  }

  def encode(node: Int): BigInt = {
    val ps = parts(node)
    val baseValue = ps.dropRight(1).zip(base.partWidths).foldRight(BigInt(0)) {
      case ((part, partWidth), suffix) => (suffix << partWidth) | part
    }
    (baseValue << childPartWidth) | ps.last
  }
}
