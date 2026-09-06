package constellation.protocol

import chisel3._
import chisel3.util._

import constellation.channel._
import constellation.noc._

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.tilelink._

/** Elaboration-time information selected by the runtime endpoint ID.
  * endpointId is Cat(topology node ID, node-local protocol port ID).
  */
case class TLEndpointContext(endpointId: Int, edge: TLEdge, idStart: Int, idSize: Int)

abstract class TLChannelToNoC[T <: TLChannel](
  gen: => T, beatBytes: Int, maxTransfer: Int, endpointIdBits: Int,
  idToEgress: Int => Int
)(implicit val p: Parameters) extends Module with TLFieldHelper {
  val flitWidth = minTLPayloadWidth(gen)
  val io = IO(new Bundle {
    val endpoint_id = Input(UInt(endpointIdBits.W))
    val protocol = Flipped(Decoupled(gen))
    val flit = Decoupled(new IngressFlit(flitWidth))
  })
  def unique(x: Seq[Boolean]): Bool = (x.count(identity) <= 1).B

  val q = Module(new Queue(gen, 1, pipe=true, flow=true))
  val protocol = q.io.deq
  def channelHasData: Bool
  def requestTargets: Seq[(Bool, Int)]

  val has_body = Wire(Bool())
  val body_fields = getBodyFields(protocol.bits)
  val const_fields = getConstFields(protocol.bits)

  // Equivalent to TLEdge.first/last, but parameterized by the union maximum
  // so a clone does not inherit one endpoint's transfer-size constants.
  private val maxLgSize = log2Ceil(maxTransfer)
  private def beatsFor(size: UInt): UInt =
    if (maxLgSize == 0) 0.U
    else UIntToOH1(size, maxLgSize) >> log2Ceil(beatBytes)
  private val beats1 = protocol.bits match {
    case bundle: TLBundleA => beatsFor(bundle.size)
    case bundle: TLBundleB => beatsFor(bundle.size)
    case bundle: TLBundleC => beatsFor(bundle.size)
    case bundle: TLBundleD => beatsFor(bundle.size)
    case _: TLBundleE => 0.U
  }
  private val packetBeats1 = Mux(channelHasData, beats1, 0.U)
  private val beatCounter = RegInit(0.U(log2Up(maxTransfer / beatBytes).W))
  private val beatCounter1 = beatCounter - 1.U
  val head = beatCounter === 0.U
  val tail = beatCounter === 1.U || packetBeats1 === 0.U
  when (protocol.fire) { beatCounter := Mux(head, packetBeats1, beatCounter1) }

  val body  = Cat(body_fields.filter(_.getWidth > 0).map(_.asUInt))
  val const = Cat(const_fields.filter(_.getWidth > 0).map(_.asUInt))
  val is_body = RegInit(false.B)
  io.flit.valid := protocol.valid
  protocol.ready := io.flit.ready && (is_body || !has_body)
  io.flit.bits.head := head && !is_body
  io.flit.bits.tail := tail && (is_body || !has_body)
  io.flit.bits.egress_id := Mux1H(requestTargets.map { case (request, target) =>
    request -> idToEgress(target).U
  })
  io.flit.bits.payload := Mux(is_body, body, const)
  when (io.flit.fire && io.flit.bits.head) { is_body := true.B }
  when (io.flit.fire && io.flit.bits.tail) { is_body := false.B }

  protected def endpointMatch(context: TLEndpointContext): Bool =
    io.endpoint_id === context.endpointId.U
}

abstract class TLChannelFromNoC[T <: TLChannel](gen: => T, endpointIdBits: Int)
  (implicit val p: Parameters) extends Module with TLFieldHelper {
  val flitWidth = minTLPayloadWidth(gen)
  val io = IO(new Bundle {
    val endpoint_id = Input(UInt(endpointIdBits.W))
    val protocol = Decoupled(gen)
    val flit = Flipped(Decoupled(new EgressFlit(flitWidth)))
  })
  def trim(id: UInt, size: Int): UInt = if (size <= 1) 0.U else id(log2Ceil(size)-1, 0)
  def trimByEndpoint(id: UInt, contexts: Seq[TLEndpointContext]): UInt =
    Mux1H(contexts.map(c => (io.endpoint_id === c.endpointId.U) -> trim(id, c.idSize)))

  val protocol = Wire(Decoupled(gen))
  val body_fields = getBodyFields(protocol.bits)
  val const_fields = getConstFields(protocol.bits)
  val is_const = RegInit(true.B)
  val const_reg = Reg(UInt(const_fields.map(_.getWidth).sum.W))
  val const = Mux(io.flit.bits.head, io.flit.bits.payload, const_reg)
  io.flit.ready := (is_const && !io.flit.bits.tail) || protocol.ready
  protocol.valid := (!is_const || io.flit.bits.tail) && io.flit.valid
  def assign(i: UInt, sigs: Seq[Data]) = {
    var t = i
    for (s <- sigs.reverse) {
      s := t.asTypeOf(s.cloneType)
      t = t >> s.getWidth
    }
  }
  assign(const, const_fields)
  assign(io.flit.bits.payload, body_fields)
  when (io.flit.fire && io.flit.bits.head) { is_const := false.B; const_reg := io.flit.bits.payload }
  when (io.flit.fire && io.flit.bits.tail) { is_const := true.B }
}

trait HasAddressDecoder {
  def filter[T](data: Seq[T], mask: Seq[Boolean]) =
    (data zip mask).filter(_._2).map(_._1)
  def reachableIO(edgeIn: TLEdge, edgesOut: Seq[TLEdge]) = edgesOut.map { mp =>
    edgeIn.client.clients.exists { c => mp.manager.managers.exists { m =>
      c.visibility.exists { ca => m.address.exists { ma => ca.overlaps(ma) } }
    } }
  }.toVector
  def releaseIO(edgeIn: TLEdge, edgesOut: Seq[TLEdge]) =
    (edgesOut zip reachableIO(edgeIn, edgesOut)).map { case (mp, reachable) =>
      reachable && edgeIn.client.anySupportProbe && mp.manager.anySupportAcquireB
    }.toVector
  def outputPortFn(edgesOut: Seq[TLEdge], connectIO: Seq[Boolean]) = {
    val portAddrs = edgesOut.map(_.manager.managers.flatMap(_.address))
    val routingMask = AddressDecoder(filter(portAddrs, connectIO))
    val routeAddrs = portAddrs.map(seq =>
      AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))
    routeAddrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_||_))
  }
}

class TLAToNoC(contexts: Seq[TLEndpointContext], edgesOut: Seq[TLEdge],
  bundle: TLBundleParameters, endpointIdBits: Int, slaveToAEgress: Int => Int)
  (implicit p: Parameters) extends TLChannelToNoC(
    new TLBundleA(bundle), edgesOut.head.manager.beatBytes,
    (contexts.map(_.edge.maxTransfer) ++ edgesOut.map(_.maxTransfer)).max,
    endpointIdBits, slaveToAEgress)(p) with HasAddressDecoder {
  def channelHasData = !protocol.bits.opcode(2)
  has_body := channelHasData || (~protocol.bits.mask =/= 0.U)
  lazy val requestTargets = contexts.flatMap { context =>
    val connect = reachableIO(context.edge, edgesOut)
    outputPortFn(edgesOut, connect).zipWithIndex.map { case (route, output) =>
      (endpointMatch(context) && connect(output).B &&
        (unique(connect) || route(protocol.bits.address))) -> output
    }
  }
  q.io.enq <> io.protocol
  q.io.enq.bits.source := io.protocol.bits.source | Mux1H(
    contexts.map(c => endpointMatch(c) -> c.idStart.U))
}

class TLAFromNoC(bundle: TLBundleParameters, endpointIdBits: Int)(implicit p: Parameters)
  extends TLChannelFromNoC(new TLBundleA(bundle), endpointIdBits)(p) {
  io.protocol <> protocol
  when (io.flit.bits.head) { io.protocol.bits.mask := ~(0.U(io.protocol.bits.mask.getWidth.W)) }
}

class TLBToNoC(contexts: Seq[TLEndpointContext], edgesIn: Seq[TLEdge],
  bundle: TLBundleParameters, endpointIdBits: Int, masterToBIngress: Int => Int)
  (implicit p: Parameters) extends TLChannelToNoC(
    new TLBundleB(bundle), contexts.head.edge.manager.beatBytes,
    (contexts.map(_.edge.maxTransfer) ++ edgesIn.map(_.maxTransfer)).max,
    endpointIdBits, masterToBIngress)(p) {
  def channelHasData = !protocol.bits.opcode(2)
  has_body := channelHasData || (~protocol.bits.mask =/= 0.U)
  lazy val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
  lazy val requestTargets = inputIdRanges.zipWithIndex.map { case (range, input) =>
    range.contains(protocol.bits.source) -> input
  }
  q.io.enq <> io.protocol
}

class TLBFromNoC(contexts: Seq[TLEndpointContext], bundle: TLBundleParameters,
  endpointIdBits: Int)(implicit p: Parameters)
  extends TLChannelFromNoC(new TLBundleB(bundle), endpointIdBits)(p) {
  io.protocol <> protocol
  io.protocol.bits.source := trimByEndpoint(protocol.bits.source, contexts)
  when (io.flit.bits.head) { io.protocol.bits.mask := ~(0.U(io.protocol.bits.mask.getWidth.W)) }
}

class TLCToNoC(contexts: Seq[TLEndpointContext], edgesOut: Seq[TLEdge],
  bundle: TLBundleParameters, endpointIdBits: Int, slaveToCEgress: Int => Int)
  (implicit p: Parameters) extends TLChannelToNoC(
    new TLBundleC(bundle), edgesOut.head.manager.beatBytes,
    (contexts.map(_.edge.maxTransfer) ++ edgesOut.map(_.maxTransfer)).max,
    endpointIdBits, slaveToCEgress)(p) with HasAddressDecoder {
  def channelHasData = protocol.bits.opcode(0)
  has_body := channelHasData
  lazy val requestTargets = contexts.flatMap { context =>
    val connect = releaseIO(context.edge, edgesOut)
    outputPortFn(edgesOut, connect).zipWithIndex.map { case (route, output) =>
      (endpointMatch(context) && connect(output).B &&
        (unique(connect) || route(protocol.bits.address))) -> output
    }
  }
  q.io.enq <> io.protocol
  q.io.enq.bits.source := io.protocol.bits.source | Mux1H(
    contexts.map(c => endpointMatch(c) -> c.idStart.U))
}

class TLCFromNoC(bundle: TLBundleParameters, endpointIdBits: Int)(implicit p: Parameters)
  extends TLChannelFromNoC(new TLBundleC(bundle), endpointIdBits)(p) {
  io.protocol <> protocol
}

class TLDToNoC(contexts: Seq[TLEndpointContext], edgesIn: Seq[TLEdge],
  bundle: TLBundleParameters, endpointIdBits: Int, masterToDIngress: Int => Int)
  (implicit p: Parameters) extends TLChannelToNoC(
    new TLBundleD(bundle), contexts.head.edge.manager.beatBytes,
    (contexts.map(_.edge.maxTransfer) ++ edgesIn.map(_.maxTransfer)).max,
    endpointIdBits, masterToDIngress)(p) {
  def channelHasData = protocol.bits.opcode(0)
  has_body := channelHasData
  lazy val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.client))
  lazy val requestTargets = inputIdRanges.zipWithIndex.map { case (range, input) =>
    range.contains(protocol.bits.source) -> input
  }
  q.io.enq <> io.protocol
  q.io.enq.bits.sink := io.protocol.bits.sink | Mux1H(
    contexts.map(c => endpointMatch(c) -> c.idStart.U))
}

class TLDFromNoC(contexts: Seq[TLEndpointContext], bundle: TLBundleParameters,
  endpointIdBits: Int)(implicit p: Parameters)
  extends TLChannelFromNoC(new TLBundleD(bundle), endpointIdBits)(p) {
  io.protocol <> protocol
  io.protocol.bits.source := trimByEndpoint(protocol.bits.source, contexts)
}

class TLEToNoC(contexts: Seq[TLEndpointContext], edgesOut: Seq[TLEdge],
  bundle: TLBundleParameters, endpointIdBits: Int, slaveToEEgress: Int => Int)
  (implicit p: Parameters) extends TLChannelToNoC(
    new TLBundleE(bundle), edgesOut.head.manager.beatBytes,
    (contexts.map(_.edge.maxTransfer) ++ edgesOut.map(_.maxTransfer)).max,
    endpointIdBits, slaveToEEgress)(p) {
  def channelHasData = false.B
  has_body := channelHasData
  lazy val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.manager))
  lazy val requestTargets = outputIdRanges.zipWithIndex.map { case (range, output) =>
    range.contains(protocol.bits.sink) -> output
  }
  q.io.enq <> io.protocol
}

class TLEFromNoC(contexts: Seq[TLEndpointContext], bundle: TLBundleParameters,
  endpointIdBits: Int)(implicit p: Parameters)
  extends TLChannelFromNoC(new TLBundleE(bundle), endpointIdBits)(p) {
  io.protocol <> protocol
  io.protocol.bits.sink := trimByEndpoint(protocol.bits.sink, contexts)
}
