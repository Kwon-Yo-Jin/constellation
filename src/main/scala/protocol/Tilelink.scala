package constellation.protocol

import chisel3._
import chisel3.util._
import chisel3.experimental.CloneModuleAsRecord

import constellation.channel._
import constellation.noc._
import constellation.soc.{CanAttachToGlobalNoC}

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._

import scala.collection.immutable.{ListMap}

trait TLFieldHelper {
  def getBodyFields(b: TLChannel): Seq[Data] = b match {
    case b: TLBundleA => Seq(b.mask, b.data, b.corrupt)
    case b: TLBundleB => Seq(b.mask, b.data, b.corrupt)
    case b: TLBundleC => Seq(        b.data, b.corrupt)
    case b: TLBundleD => Seq(        b.data, b.corrupt)
    case b: TLBundleE => Seq()
  }
  def getConstFields(b: TLChannel): Seq[Data] = b match {
    case b: TLBundleA => Seq(b.opcode, b.param, b.size, b.source, b.address, b.user, b.echo                  )
    case b: TLBundleB => Seq(b.opcode, b.param, b.size, b.source, b.address                                  )
    case b: TLBundleC => Seq(b.opcode, b.param, b.size, b.source, b.address, b.user, b.echo                  )
    case b: TLBundleD => Seq(b.opcode, b.param, b.size, b.source,            b.user, b.echo, b.sink, b.denied)
    case b: TLBundleE => Seq(                                                                b.sink          )
  }
  def minTLPayloadWidth(b: TLChannel): Int = Seq(getBodyFields(b), getConstFields(b)).map(_.map(_.getWidth).sum).max
  def minTLPayloadWidth(bs: Seq[TLChannel]): Int = bs.map(b => minTLPayloadWidth(b)).max
  def minTLPayloadWidth(b: TLBundle): Int = minTLPayloadWidth(Seq(b.a, b.b, b.c, b.d, b.e).map(_.bits))
}

class TLMasterToNoC(
  contexts: Seq[TLEndpointContext], edgesOut: Seq[TLEdge],
  endpointIdBits: Int,
  wideBundle: TLBundleParameters,
  slaveToEgressOffset: Int => Int,
  flitWidth: Int
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val endpoint_id = Input(UInt(endpointIdBits.W))
    val tilelink = Flipped(new TLBundle(wideBundle))
    val flits = new Bundle {
      val a = Decoupled(new IngressFlit(flitWidth))
      val b = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val c = Decoupled(new IngressFlit(flitWidth))
      val d = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val e = Decoupled(new IngressFlit(flitWidth))
    }
  })
  val a = Module(new TLAToNoC(contexts, edgesOut, wideBundle, endpointIdBits, (i) => slaveToEgressOffset(i) + 0))
  val b = Module(new TLBFromNoC(contexts, wideBundle, endpointIdBits))
  val c = Module(new TLCToNoC(contexts, edgesOut, wideBundle, endpointIdBits, (i) => slaveToEgressOffset(i) + 1))
  val d = Module(new TLDFromNoC(contexts, wideBundle, endpointIdBits))
  val e = Module(new TLEToNoC(contexts, edgesOut, wideBundle, endpointIdBits, (i) => slaveToEgressOffset(i) + 2))
  a.io.endpoint_id := io.endpoint_id
  b.io.endpoint_id := io.endpoint_id
  c.io.endpoint_id := io.endpoint_id
  d.io.endpoint_id := io.endpoint_id
  e.io.endpoint_id := io.endpoint_id
  a.io.protocol <> io.tilelink.a
  io.tilelink.b <> b.io.protocol
  c.io.protocol <> io.tilelink.c
  io.tilelink.d <> d.io.protocol
  e.io.protocol <> io.tilelink.e

  io.flits.a <> a.io.flit
  b.io.flit  <> io.flits.b
  io.flits.c <> c.io.flit
  d.io.flit  <> io.flits.d
  io.flits.e <> e.io.flit
}

class TLMasterACDToNoC(
  contexts: Seq[TLEndpointContext], edgesOut: Seq[TLEdge],
  endpointIdBits: Int,
  wideBundle: TLBundleParameters,
  slaveToEgressOffset: Int => Int,
  flitWidth: Int
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val endpoint_id = Input(UInt(endpointIdBits.W))
    val tilelink = Flipped(new TLBundle(wideBundle))
    val flits = new Bundle {
      val a = Decoupled(new IngressFlit(flitWidth))
      val c = Decoupled(new IngressFlit(flitWidth))
      val d = Flipped(Decoupled(new EgressFlit(flitWidth)))
    }
  })
  io.tilelink := DontCare
  val a = Module(new TLAToNoC(contexts, edgesOut, wideBundle, endpointIdBits, (i) => slaveToEgressOffset(i) + 0))
  val c = Module(new TLCToNoC(contexts, edgesOut, wideBundle, endpointIdBits, (i) => slaveToEgressOffset(i) + 1))
  val d = Module(new TLDFromNoC(contexts, wideBundle, endpointIdBits))
  a.io.endpoint_id := io.endpoint_id
  c.io.endpoint_id := io.endpoint_id
  d.io.endpoint_id := io.endpoint_id
  a.io.protocol <> io.tilelink.a
  c.io.protocol <> io.tilelink.c
  io.tilelink.d <> d.io.protocol

  io.flits.a <> a.io.flit
  io.flits.c <> c.io.flit
  d.io.flit  <> io.flits.d
}

class TLMasterBEToNoC(
  contexts: Seq[TLEndpointContext], edgesOut: Seq[TLEdge],
  endpointIdBits: Int,
  wideBundle: TLBundleParameters,
  slaveToEgressOffset: Int => Int,
  flitWidth: Int
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val endpoint_id = Input(UInt(endpointIdBits.W))
    val tilelink = Flipped(new TLBundle(wideBundle))
    val flits = new Bundle {
      val b = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val e = Decoupled(new IngressFlit(flitWidth))
    }
  })
  io.tilelink := DontCare
  val b = Module(new TLBFromNoC(contexts, wideBundle, endpointIdBits))
  val e = Module(new TLEToNoC(contexts, edgesOut, wideBundle, endpointIdBits, (i) => slaveToEgressOffset(i) + 0))
  b.io.endpoint_id := io.endpoint_id
  e.io.endpoint_id := io.endpoint_id
  io.tilelink.b <> b.io.protocol
  e.io.protocol <> io.tilelink.e

  b.io.flit  <> io.flits.b
  io.flits.e <> e.io.flit
}


class TLSlaveToNoC(
  contexts: Seq[TLEndpointContext], edgesIn: Seq[TLEdge],
  endpointIdBits: Int,
  wideBundle: TLBundleParameters,
  masterToEgressOffset: Int => Int,
  flitWidth: Int
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val endpoint_id = Input(UInt(endpointIdBits.W))
    val tilelink = new TLBundle(wideBundle)
    val flits = new Bundle {
      val a = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val b = Decoupled(new IngressFlit(flitWidth))
      val c = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val d = Decoupled(new IngressFlit(flitWidth))
      val e = Flipped(Decoupled(new EgressFlit(flitWidth)))
    }
  })

  val a = Module(new TLAFromNoC(wideBundle, endpointIdBits))
  val b = Module(new TLBToNoC(contexts, edgesIn, wideBundle, endpointIdBits, (i) => masterToEgressOffset(i) + 0))
  val c = Module(new TLCFromNoC(wideBundle, endpointIdBits))
  val d = Module(new TLDToNoC(contexts, edgesIn, wideBundle, endpointIdBits, (i) => masterToEgressOffset(i) + 1))
  val e = Module(new TLEFromNoC(contexts, wideBundle, endpointIdBits))
  a.io.endpoint_id := io.endpoint_id
  b.io.endpoint_id := io.endpoint_id
  c.io.endpoint_id := io.endpoint_id
  d.io.endpoint_id := io.endpoint_id
  e.io.endpoint_id := io.endpoint_id
  io.tilelink.a <> a.io.protocol
  b.io.protocol <> io.tilelink.b
  io.tilelink.c <> c.io.protocol
  d.io.protocol <> io.tilelink.d
  io.tilelink.e <> e.io.protocol

  a.io.flit  <> io.flits.a
  io.flits.b <> b.io.flit
  c.io.flit  <> io.flits.c
  io.flits.d <> d.io.flit
  e.io.flit  <> io.flits.e
}

class TLSlaveACDToNoC(
  contexts: Seq[TLEndpointContext], edgesIn: Seq[TLEdge],
  endpointIdBits: Int,
  wideBundle: TLBundleParameters,
  masterToEgressOffset: Int => Int,
  flitWidth: Int
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val endpoint_id = Input(UInt(endpointIdBits.W))
    val tilelink = new TLBundle(wideBundle)
    val flits = new Bundle {
      val a = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val c = Flipped(Decoupled(new EgressFlit(flitWidth)))
      val d = Decoupled(new IngressFlit(flitWidth))
    }
  })
  io.tilelink := DontCare
  val a = Module(new TLAFromNoC(wideBundle, endpointIdBits))
  val c = Module(new TLCFromNoC(wideBundle, endpointIdBits))
  val d = Module(new TLDToNoC(contexts, edgesIn, wideBundle, endpointIdBits, (i) => masterToEgressOffset(i) + 0))
  a.io.endpoint_id := io.endpoint_id
  c.io.endpoint_id := io.endpoint_id
  d.io.endpoint_id := io.endpoint_id
  io.tilelink.a <> a.io.protocol
  io.tilelink.c <> c.io.protocol
  d.io.protocol <> io.tilelink.d

  a.io.flit  <> io.flits.a
  c.io.flit  <> io.flits.c
  io.flits.d <> d.io.flit
}

class TLSlaveBEToNoC(
  contexts: Seq[TLEndpointContext], edgesIn: Seq[TLEdge],
  endpointIdBits: Int,
  wideBundle: TLBundleParameters,
  masterToEgressOffset: Int => Int,
  flitWidth: Int
)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val endpoint_id = Input(UInt(endpointIdBits.W))
    val tilelink = new TLBundle(wideBundle)
    val flits = new Bundle {
      val b = Decoupled(new IngressFlit(flitWidth))
      val e = Flipped(Decoupled(new EgressFlit(flitWidth)))
    }
  })
  io.tilelink := DontCare
  val b = Module(new TLBToNoC(contexts, edgesIn, wideBundle, endpointIdBits, (i) => masterToEgressOffset(i) + 0))
  val e = Module(new TLEFromNoC(contexts, wideBundle, endpointIdBits))
  b.io.endpoint_id := io.endpoint_id
  e.io.endpoint_id := io.endpoint_id
  b.io.protocol <> io.tilelink.b
  io.tilelink.e <> e.io.protocol

  io.flits.b <> b.io.flit
  e.io.flit  <> io.flits.e
}

class TileLinkInterconnectInterface(edgesIn: Seq[TLEdge], edgesOut: Seq[TLEdge])(implicit val p: Parameters) extends Bundle {
  val in = MixedVec(edgesIn.map { e => Flipped(new TLBundle(e.bundle)) })
  val out = MixedVec(edgesOut.map { e => new TLBundle(e.bundle) })
}

trait TileLinkProtocolParams extends ProtocolParams with TLFieldHelper {
  def edgesIn: Seq[TLEdge]
  def edgesOut: Seq[TLEdge]
  def edgeInNodes: Seq[Int]
  def edgeOutNodes: Seq[Int]
  require(edgesIn.size == edgeInNodes.size && edgesOut.size == edgeOutNodes.size)
  def wideBundle = TLBundleParameters.union(edgesIn.map(_.bundle) ++ edgesOut.map(_.bundle))
  def genBundle = new TLBundle(wideBundle)
  def inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
  def outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))

  private def localPortIds(nodes: Seq[Int]): Seq[Int] = nodes.indices.map { i =>
    nodes.take(i).count(_ == nodes(i))
  }
  private def maxPortsPerNode(nodes: Seq[Int]): Int =
    nodes.groupBy(identity).values.map(_.size).foldLeft(1)(math.max)
  def endpointPortBits: Int = log2Up(math.max(
    maxPortsPerNode(edgeInNodes), maxPortsPerNode(edgeOutNodes)))
  def endpointNodeBits: Int = log2Up((edgeInNodes ++ edgeOutNodes).max + 1)
  def endpointIdBits: Int = endpointNodeBits + endpointPortBits
  private def encodedEndpointIds(nodes: Seq[Int]): Seq[Int] = {
    val ids = nodes.zip(localPortIds(nodes)).map { case (nodeId, portId) =>
      (nodeId << endpointPortBits) | portId
    }
    require(ids.distinct.size == ids.size)
    ids
  }
  def masterEndpointContexts: Seq[TLEndpointContext] =
    encodedEndpointIds(edgeInNodes).lazyZip(edgesIn).lazyZip(inputIdRanges).map {
      case (endpointId, edge, range) =>
        TLEndpointContext(endpointId, edge, range.start, range.size)
    }
  def slaveEndpointContexts: Seq[TLEndpointContext] =
    encodedEndpointIds(edgeOutNodes).lazyZip(edgesOut).lazyZip(outputIdRanges).map {
      case (endpointId, edge, range) =>
        TLEndpointContext(endpointId, edge, range.start, range.size)
    }
  def masterEndpointId(index: Int): UInt =
    Cat(edgeInNodes(index).U(endpointNodeBits.W),
      localPortIds(edgeInNodes)(index).U(endpointPortBits.W))
  def slaveEndpointId(index: Int): UInt =
    Cat(edgeOutNodes(index).U(endpointNodeBits.W),
      localPortIds(edgeOutNodes)(index).U(endpointPortBits.W))

  val vNetBlocking = (blocker: Int, blockee: Int) => blocker < blockee

  def genIO()(implicit p: Parameters): Data = new TileLinkInterconnectInterface(edgesIn, edgesOut)
}

object TLConnect {
  def apply[T <: TLBundleBase](l: DecoupledIO[T], r: DecoupledIO[T]) = {
    l.valid := r.valid
    r.ready := l.ready
    l.bits.squeezeAll.waiveAll :<>= r.bits.squeezeAll.waiveAll
  }
}

// BEGIN: TileLinkProtocolParams
case class TileLinkABCDEProtocolParams(
  edgesIn: Seq[TLEdge],
  edgesOut: Seq[TLEdge],
  edgeInNodes: Seq[Int],
  edgeOutNodes: Seq[Int]
) extends TileLinkProtocolParams {
  // END: TileLinkProtocolParams
  val minPayloadWidth = minTLPayloadWidth(new TLBundle(wideBundle))
  val ingressNodes = (edgeInNodes.map(u => Seq.fill(3) (u)) ++ edgeOutNodes.map(u => Seq.fill (2) {u})).flatten
  val egressNodes = (edgeInNodes.map(u => Seq.fill(2) (u)) ++ edgeOutNodes.map(u => Seq.fill (3) {u})).flatten
  val nVirtualNetworks = 5

  val flows = edgesIn.zipWithIndex.map { case (edgeIn, ii) => edgesOut.zipWithIndex.map { case (edgeOut, oi) =>
    val reachable = edgeIn.client.clients.exists { c => edgeOut.manager.managers.exists { m =>
      c.visibility.exists { ca => m.address.exists { ma =>
        ca.overlaps(ma)
      }}
    }}
    val probe = edgeIn.client.anySupportProbe && edgeOut.manager.managers.exists(_.regionType >= RegionType.TRACKED)
    val release = edgeIn.client.anySupportProbe && edgeOut.manager.anySupportAcquireB
    ( (if (reachable) Some(FlowParams(ii * 3 + 0                   , oi * 3 + 0 + edgesIn.size * 2, 4)) else None) ++ // A
      (if (probe    ) Some(FlowParams(oi * 2 + 0 + edgesIn.size * 3, ii * 2 + 0                   , 3)) else None) ++ // B
      (if (release  ) Some(FlowParams(ii * 3 + 1                   , oi * 3 + 1 + edgesIn.size * 2, 2)) else None) ++ // C
      (if (reachable) Some(FlowParams(oi * 2 + 1 + edgesIn.size * 3, ii * 2 + 1                   , 1)) else None) ++ // D
      (if (release  ) Some(FlowParams(ii * 3 + 2                   , oi * 3 + 2 + edgesIn.size * 2, 0)) else None))   // E
  }}.flatten.flatten

  def interface(terminals: NoCTerminalIO,
    ingressOffset: Int, egressOffset: Int, protocol: Data)(implicit p: Parameters) = {
    val ingresses = terminals.ingress
    val egresses = terminals.egress
    protocol match { case protocol: TileLinkInterconnectInterface => {
      val masterPrototype = Module(new TLMasterToNoC(
        masterEndpointContexts, edgesOut, endpointIdBits,
        wideBundle,
        (s) => s * 3 + edgesIn.size * 2 + egressOffset,
        minPayloadWidth
      ))
      edgesIn.zipWithIndex.map { case (e,i) =>
        val nif_master = if (i == 0) masterPrototype.io else
          CloneModuleAsRecord(masterPrototype).apply("io").asInstanceOf[masterPrototype.io.type]
        nif_master.endpoint_id := masterEndpointId(i)
        nif_master.tilelink := DontCare
        nif_master.tilelink.a.valid := false.B
        nif_master.tilelink.c.valid := false.B
        nif_master.tilelink.e.valid := false.B

        TLConnect(nif_master.tilelink.a, protocol.in(i).a)
        TLConnect(protocol.in(i).d, nif_master.tilelink.d)

        if (protocol.in(i).params.hasBCE) {
          TLConnect(protocol.in(i).b, nif_master.tilelink.b)
          TLConnect(nif_master.tilelink.c, protocol.in(i).c)
          TLConnect(nif_master.tilelink.e, protocol.in(i).e)
        }

        ingresses(i * 3 + 0).flit <> nif_master.flits.a
        ingresses(i * 3 + 1).flit <> nif_master.flits.c
        ingresses(i * 3 + 2).flit <> nif_master.flits.e
        nif_master.flits.b <> egresses(i * 2 + 0).flit
        nif_master.flits.d <> egresses(i * 2 + 1).flit
      }
      val slavePrototype = Module(new TLSlaveToNoC(
        slaveEndpointContexts, edgesIn, endpointIdBits,
        wideBundle,
        (s) => s * 2 + egressOffset,
        minPayloadWidth
      ))
      edgesOut.zipWithIndex.map { case (e,i) =>
        val nif_slave = if (i == 0) slavePrototype.io else
          CloneModuleAsRecord(slavePrototype).apply("io").asInstanceOf[slavePrototype.io.type]
        nif_slave.endpoint_id := slaveEndpointId(i)
        nif_slave.tilelink := DontCare
        nif_slave.tilelink.b.valid := false.B
        nif_slave.tilelink.d.valid := false.B

        TLConnect(protocol.out(i).a, nif_slave.tilelink.a)
        TLConnect(nif_slave.tilelink.d, protocol.out(i).d)

        if (protocol.out(i).params.hasBCE) {
          TLConnect(nif_slave.tilelink.b, protocol.out(i).b)
          TLConnect(protocol.out(i).c, nif_slave.tilelink.c)
          TLConnect(protocol.out(i).e, nif_slave.tilelink.e)
        }

        ingresses(i * 2 + 0 + edgesIn.size * 3).flit <> nif_slave.flits.b
        ingresses(i * 2 + 1 + edgesIn.size * 3).flit <> nif_slave.flits.d
        nif_slave.flits.a <> egresses(i * 3 + 0 + edgesIn.size * 2).flit
        nif_slave.flits.c <> egresses(i * 3 + 1 + edgesIn.size * 2).flit
        nif_slave.flits.e <> egresses(i * 3 + 2 + edgesIn.size * 2).flit
      }
    } }
  }
}

case class TileLinkACDProtocolParams(
  edgesIn: Seq[TLEdge],
  edgesOut: Seq[TLEdge],
  edgeInNodes: Seq[Int],
  edgeOutNodes: Seq[Int]) extends TileLinkProtocolParams {
  val minPayloadWidth = minTLPayloadWidth(Seq(genBundle.a, genBundle.c, genBundle.d).map(_.bits))
  val ingressNodes = (edgeInNodes.map(u => Seq.fill(2) (u)) ++ edgeOutNodes.map(u => Seq.fill (1) {u})).flatten
  val egressNodes = (edgeInNodes.map(u => Seq.fill(1) (u)) ++ edgeOutNodes.map(u => Seq.fill (2) {u})).flatten

  val nVirtualNetworks = 3
  val flows = edgesIn.zipWithIndex.map { case (edgeIn, ii) => edgesOut.zipWithIndex.map { case (edgeOut, oi) =>
    val reachable = edgeIn.client.clients.exists { c => edgeOut.manager.managers.exists { m =>
      c.visibility.exists { ca => m.address.exists { ma =>
        ca.overlaps(ma)
      }}
    }}
    val release = edgeIn.client.anySupportProbe && edgeOut.manager.anySupportAcquireB
    ( (if (reachable) Some(FlowParams(ii * 2 + 0                   , oi * 2 + 0 + edgesIn.size * 1, 2)) else None) ++ // A
      (if (release  ) Some(FlowParams(ii * 2 + 1                   , oi * 2 + 1 + edgesIn.size * 1, 1)) else None) ++ // C
      (if (reachable) Some(FlowParams(oi * 1 + 0 + edgesIn.size * 2, ii * 1 + 0                   , 0)) else None))   // D

  }}.flatten.flatten


  def interface(terminals: NoCTerminalIO,
    ingressOffset: Int, egressOffset: Int, protocol: Data)(implicit p: Parameters) = {
    val ingresses = terminals.ingress
    val egresses = terminals.egress
    protocol match { case protocol: TileLinkInterconnectInterface => {
      protocol := DontCare
      val masterPrototype = Module(new TLMasterACDToNoC(
        masterEndpointContexts, edgesOut, endpointIdBits,
        wideBundle,
        (s) => s * 2 + edgesIn.size * 1 + egressOffset,
        minPayloadWidth
      ))
      edgesIn.zipWithIndex.map { case (e,i) =>
        val nif_master_acd = if (i == 0) masterPrototype.io else
          CloneModuleAsRecord(masterPrototype).apply("io").asInstanceOf[masterPrototype.io.type]
        nif_master_acd.endpoint_id := masterEndpointId(i)
        nif_master_acd.tilelink := DontCare
        nif_master_acd.tilelink.a.valid := false.B
        nif_master_acd.tilelink.c.valid := false.B
        nif_master_acd.tilelink.e.valid := false.B

        TLConnect(nif_master_acd.tilelink.a, protocol.in(i).a)
        TLConnect(protocol.in(i).d, nif_master_acd.tilelink.d)

        if (protocol.in(i).params.hasBCE) {
          TLConnect(nif_master_acd.tilelink.c, protocol.in(i).c)
        }

        ingresses(i * 2 + 0).flit <> nif_master_acd.flits.a
        ingresses(i * 2 + 1).flit <> nif_master_acd.flits.c
        nif_master_acd.flits.d <> egresses(i * 1 + 0).flit
      }
      val slavePrototype = Module(new TLSlaveACDToNoC(
        slaveEndpointContexts, edgesIn, endpointIdBits,
        wideBundle,
        (s) => s * 1 + egressOffset,
        minPayloadWidth
      ))
      edgesOut.zipWithIndex.map { case (e,i) =>
        val nif_slave_acd = if (i == 0) slavePrototype.io else
          CloneModuleAsRecord(slavePrototype).apply("io").asInstanceOf[slavePrototype.io.type]
        nif_slave_acd.endpoint_id := slaveEndpointId(i)
        nif_slave_acd.tilelink := DontCare
        nif_slave_acd.tilelink.b.valid := false.B
        nif_slave_acd.tilelink.d.valid := false.B

        TLConnect(protocol.out(i).a, nif_slave_acd.tilelink.a)
        TLConnect(nif_slave_acd.tilelink.d, protocol.out(i).d)

        if (protocol.out(i).params.hasBCE) {
          TLConnect(protocol.out(i).c, nif_slave_acd.tilelink.c)
        }

        ingresses(i * 1 + 0 + edgesIn.size * 2).flit <> nif_slave_acd.flits.d
        nif_slave_acd.flits.a <> egresses(i * 2 + 0 + edgesIn.size * 1).flit
        nif_slave_acd.flits.c <> egresses(i * 2 + 1 + edgesIn.size * 1).flit
      }
    }}
  }
}

case class TileLinkBEProtocolParams(
  edgesIn: Seq[TLEdge],
  edgesOut: Seq[TLEdge],
  edgeInNodes: Seq[Int],
  edgeOutNodes: Seq[Int]) extends TileLinkProtocolParams {
  val minPayloadWidth = minTLPayloadWidth(Seq(genBundle.b, genBundle.e).map(_.bits))
  val ingressNodes = (edgeInNodes.map(u => Seq.fill(1) (u)) ++ edgeOutNodes.map(u => Seq.fill (1) {u})).flatten
  val egressNodes = (edgeInNodes.map(u => Seq.fill(1) (u)) ++ edgeOutNodes.map(u => Seq.fill (1) {u})).flatten

  val nVirtualNetworks = 2
  val flows = edgesIn.zipWithIndex.map { case (edgeIn, ii) => edgesOut.zipWithIndex.map { case (edgeOut, oi) =>
    val probe = edgeIn.client.anySupportProbe && edgeOut.manager.managers.exists(_.regionType >= RegionType.TRACKED)
    val release = edgeIn.client.anySupportProbe && edgeOut.manager.anySupportAcquireB
    ( (if (probe    ) Some(FlowParams(oi * 1 + 0 + edgesIn.size * 1, ii * 1 + 0                   , 1)) else None) ++ // B
      (if (release  ) Some(FlowParams(ii * 1 + 0                   , oi * 1 + 0 + edgesIn.size * 1, 0)) else None))   // E
  }}.flatten.flatten

  def interface(terminals: NoCTerminalIO,
    ingressOffset: Int, egressOffset: Int, protocol: Data)(implicit p: Parameters) = {
    val ingresses = terminals.ingress
    val egresses = terminals.egress
    protocol match { case protocol: TileLinkInterconnectInterface => {
      protocol := DontCare
      val masterPrototype = Module(new TLMasterBEToNoC(
        masterEndpointContexts, edgesOut, endpointIdBits,
        wideBundle,
        (s) => s * 1 + edgesIn.size * 1 + egressOffset,
        minPayloadWidth
      ))
      edgesIn.zipWithIndex.map { case (e,i) =>
        val nif_master_be = if (i == 0) masterPrototype.io else
          CloneModuleAsRecord(masterPrototype).apply("io").asInstanceOf[masterPrototype.io.type]
        nif_master_be.endpoint_id := masterEndpointId(i)
        nif_master_be.tilelink := DontCare
        nif_master_be.tilelink.a.valid := false.B
        nif_master_be.tilelink.c.valid := false.B
        nif_master_be.tilelink.e.valid := false.B

        if (protocol.in(i).params.hasBCE) {
          TLConnect(protocol.in(i).b, nif_master_be.tilelink.b)
          TLConnect(nif_master_be.tilelink.e, protocol.in(i).e)
        }

        ingresses(i * 1 + 0).flit <> nif_master_be.flits.e
        nif_master_be.flits.b <> egresses(i * 1 + 0).flit
      }
      val slavePrototype = Module(new TLSlaveBEToNoC(
        slaveEndpointContexts, edgesIn, endpointIdBits,
        wideBundle,
        (s) => s * 1 + egressOffset,
        minPayloadWidth
      ))
      edgesOut.zipWithIndex.map { case (e,i) =>
        val nif_slave_be = if (i == 0) slavePrototype.io else
          CloneModuleAsRecord(slavePrototype).apply("io").asInstanceOf[slavePrototype.io.type]
        nif_slave_be.endpoint_id := slaveEndpointId(i)
        nif_slave_be.tilelink := DontCare
        nif_slave_be.tilelink.b.valid := false.B
        nif_slave_be.tilelink.d.valid := false.B

        if (protocol.out(i).params.hasBCE) {
          TLConnect(protocol.out(i).e, nif_slave_be.tilelink.e)
          TLConnect(nif_slave_be.tilelink.b, protocol.out(i).b)
        }

        ingresses(i * 1 + 0 + edgesIn.size * 1).flit <> nif_slave_be.flits.b
        nif_slave_be.flits.e <> egresses(i * 1 + 0 + edgesIn.size * 1).flit
      }
    }}
  }
}

abstract class TLNoCLike(implicit p: Parameters) extends LazyModule {
  val node = new TLNexusNode(
    clientFn  = { seq =>
      seq(0).v1copy(
        echoFields    = BundleField.union(seq.flatMap(_.echoFields)),
        requestFields = BundleField.union(seq.flatMap(_.requestFields)),
        responseKeys  = seq.flatMap(_.responseKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        clients = (TLXbar.mapInputIds(seq) zip seq) flatMap { case (range, port) =>
          port.clients map { client => client.v1copy(
            sourceId = client.sourceId.shift(range.start)
          )}
        }
      )
    },
    managerFn = { seq =>
      val fifoIdFactory = TLXbar.relabeler()
      seq(0).v1copy(
        responseFields = BundleField.union(seq.flatMap(_.responseFields)),
        requestKeys = seq.flatMap(_.requestKeys).distinct,
        minLatency = seq.map(_.minLatency).min,
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        managers = seq.flatMap { port =>
          require (port.beatBytes == seq(0).beatBytes,
            s"TLNoC (data widths don't match: ${port.managers.map(_.name)} has ${port.beatBytes}B vs ${seq(0).managers.map(_.name)} has ${seq(0).beatBytes}B")
          // TileLink NoC does not preserve FIFO-ness, masters to this NoC should instantiate FIFOFixers
          port.managers map { manager => manager.v1copy(fifoId = None) }
        }
      )
    }
  )
}

abstract class TLNoCModuleImp(outer: LazyModule) extends LazyModuleImp(outer) {
  val edgesIn: Seq[TLEdge]
  val edgesOut: Seq[TLEdge]
  val nodeMapping: DiplomaticNetworkNodeMapping
  val nocName: String
  lazy val inNames  = nodeMapping.genUniqueName(edgesIn.map(_.master.masters.map(_.name)))
  lazy val outNames = nodeMapping.genUniqueName(edgesOut.map(_.slave.slaves.map(_.name)))
  lazy val edgeInNodes = nodeMapping.getNodesIn(inNames)
  lazy val edgeOutNodes = nodeMapping.getNodesOut(outNames)
  def printNodeMappings() {
    println(s"Constellation: TLNoC $nocName inwards mapping:")
    for ((n, i) <- inNames zip edgeInNodes) {
      val node = i.map(_.toString).getOrElse("X")
      println(s"  $node <- $n")
    }

    println(s"Constellation: TLNoC $nocName outwards mapping:")
    for ((n, i) <- outNames zip edgeOutNodes) {
      val node = i.map(_.toString).getOrElse("X")
      println(s"  $node <- $n")
    }
  }
}

trait TLNoCParams

// Instantiates a private TLNoC. Replaces the TLXbar
// BEGIN: TLNoCParams
case class SimpleTLNoCParams(
  nodeMappings: DiplomaticNetworkNodeMapping,
  nocParams: NoCParams = NoCParams(),
) extends TLNoCParams
class TLNoC(params: SimpleTLNoCParams, name: String = "test", inlineNoC: Boolean = false)(implicit p: Parameters) extends TLNoCLike {
  // END: TLNoCParams

  override def shouldBeInlined = inlineNoC
  lazy val module = new TLNoCModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val nodeMapping = params.nodeMappings
    val nocName = name

    printNodeMappings()

    val protocolParams = TileLinkABCDEProtocolParams(
      edgesIn = edgesIn,
      edgesOut = edgesOut,
      edgeInNodes = edgeInNodes.flatten,
      edgeOutNodes = edgeOutNodes.flatten
    )

    val noc = Module(new ProtocolNoC(ProtocolNoCParams(
      params.nocParams.copy(hasCtrl = false, nocName=name, inlineNoC = inlineNoC),
      Seq(protocolParams),
      inlineNoC = inlineNoC
    )))

    noc.io.protocol(0) match {
      case protocol: TileLinkInterconnectInterface => {
        (protocol.in zip io_in).foreach { case (l,r) => l <> r }
        (io_out zip protocol.out).foreach { case (l,r) => l <> r }
      }
    }
  }
}

case class SplitACDxBETLNoCParams(
  nodeMappings: DiplomaticNetworkNodeMapping,
  acdNoCParams: NoCParams = NoCParams(),
  beNoCParams: NoCParams = NoCParams(),
  beDivision: Int = 2
) extends TLNoCParams
class TLSplitACDxBENoC(params: SplitACDxBETLNoCParams, name: String = "test", inlineNoC: Boolean = false)(implicit p: Parameters) extends TLNoCLike {
  override def shouldBeInlined = inlineNoC
  lazy val module = new TLNoCModuleImp(this) {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val nodeMapping = params.nodeMappings
    val nocName = name

    printNodeMappings()

    val acdProtocolParams = TileLinkACDProtocolParams(
      edgesIn = edgesIn,
      edgesOut = edgesOut,
      edgeInNodes = edgeInNodes.flatten,
      edgeOutNodes = edgeOutNodes.flatten
    )
    val beProtocolParams = TileLinkBEProtocolParams(
      edgesIn = edgesIn,
      edgesOut = edgesOut,
      edgeInNodes = edgeInNodes.flatten,
      edgeOutNodes = edgeOutNodes.flatten
    )

    val acd_noc = Module(new ProtocolNoC(ProtocolNoCParams(
      params.acdNoCParams.copy(hasCtrl = false, nocName=s"${name}_acd", inlineNoC = inlineNoC),
      Seq(acdProtocolParams),
      inlineNoC = inlineNoC
    )))
    val be_noc = Module(new ProtocolNoC(ProtocolNoCParams(
      params.beNoCParams.copy(hasCtrl = false, nocName=s"${name}_be", inlineNoC = inlineNoC),
      Seq(beProtocolParams),
      widthDivision = params.beDivision,
      inlineNoC = inlineNoC
    )))

    acd_noc.io.protocol(0) match { case protocol: TileLinkInterconnectInterface => {
      (protocol.in zip io_in).foreach { case (l,r) =>
        l := DontCare
        l.a <> r.a
        l.c <> r.c
        l.d <> r.d
      }
      (io_out zip protocol.out).foreach { case (l,r) =>
        r := DontCare
        l.a <> r.a
        l.c <> r.c
        l.d <> r.d
      }
    }}

    be_noc.io.protocol(0) match { case protocol: TileLinkInterconnectInterface => {
      (protocol.in zip io_in).foreach { case (l,r) =>
        l := DontCare
        l.b <> r.b
        l.e <> r.e
      }
      (io_out zip protocol.out).foreach { case (l,r) =>
        r := DontCare
        l.b <> r.b
        l.e <> r.e
      }
    }}
  }
}

case class GlobalTLNoCParams(
  nodeMappings: DiplomaticNetworkNodeMapping
) extends TLNoCParams
// Maps this interconnect onto a global NoC
class TLGlobalNoC(params: GlobalTLNoCParams, name: String = "test")(implicit p: Parameters) extends TLNoCLike {
  lazy val module = new TLNoCModuleImp(this) with CanAttachToGlobalNoC {
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip
    val nodeMapping = params.nodeMappings
    val nocName = name

    val protocolParams = TileLinkABCDEProtocolParams(
      edgesIn = edgesIn,
      edgesOut = edgesOut,
      edgeInNodes = edgeInNodes.flatten,
      edgeOutNodes = edgeOutNodes.flatten
    )

    printNodeMappings()
    val io_global = IO(Flipped(protocolParams.genIO()))
    io_global match {
      case protocol: TileLinkInterconnectInterface => {
        (protocol.in zip io_in).foreach { case (l,r) => l <> r }
        (io_out zip protocol.out).foreach { case (l,r) => l <> r }
      }
    }

  }
}
