package dma

import chisel3._
import chisel3.util._
import chisel3.util.random._
import chisel3.experimental.chiselName

import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util._
import testchipip.TLHelper

case class DMAParams(
    //TODO split into multiple appropriate Config classes later
    temp: Boolean,
    dataWidth: Int = 32, 
    addrWidth: Int = 32,
    addrCtrlWidth: Int = 16,
    idWidth: Int = 8,
    txnIdWidth:Int = 4,
    beatBytes: Int = 32,
    maxBurst: Int = 4,
    fifoDepth: Int = 512,
    nOutstanding: Int = 4,
    maxDMAReqs: Int = 4) {
  //derive from top config?
  val busWidth = beatBytes * 8
  val wordsPerBeat = busWidth/dataWidth
  val maxWords = maxBurst * wordsPerBeat
  val maxBytes = maxBurst * beatBytes
  val lgMaxBytes = log2Up(maxBytes)

}

case object DSMKey extends Field[DSMParams]
case object DMAKey extends Field[DMAParams]

class WithDSM(in:Boolean) extends Config((site, here, up) => {
  case DSMKey => DSMParams(temp = in)
})
class WithDMA(in: Boolean) extends Config((site, here, up) => {
  case DMAKey => DMAParams(temp = in)
})

class PortParam (implicit p: Parameters) extends Bundle {
  val c = p(DMAKey)
  //XXX currently xStep is ignored (1 is the default)
  val xStep, yStep = UInt(c.addrCtrlWidth.W)
  val xCnt, yCnt   = UInt(c.addrCtrlWidth.W)
  val addr         = UInt(c.addrWidth.W)
  val nodeId       = UInt(c.idWidth.W)
  
  override def cloneType = (new PortParam).asInstanceOf[this.type]
}

class CSRBundle(implicit p: Parameters) extends Bundle {
  val src    = new PortParam
  val dest   = new PortParam
  //val txnId  = UInt(p(DMAKey).txnIdWidth.W)
  val mode   = Bool()//choose false: noc->dsm or true: dsm->noc
  val start  = Bool()

  override def cloneType = (new CSRBundle).asInstanceOf[this.type]
}

class DMA(implicit p: Parameters) extends LazyModule {
  val size = p(DSMKey).dsmSize
  val noc = TLHelper.makeClientNode(TLMasterParameters.v1(
    name = "dmaSlaveToNoC",
    sourceId = IdRange(0, 16),
    requestFifo = true,
    visibility = Seq(AddressSet(0x0, 0xffffff))))

  val dsm = TLHelper.makeClientNode(TLMasterParameters.v1(
    name = "dmaSlaveToDSM",
    sourceId = IdRange(0, 16),
    requestFifo = true,
    visibility = Seq(AddressSet(0x0, 0xffffff))))
  
  lazy val module = new DMAModule(this)
}

@chiselName
class DMAModule(outer: DMA) extends LazyModuleImp(outer) {
  val c = p(DMAKey)
  val (noc, nocEdge) = outer.noc.out(0)
  val (dsm, dsmEdge) = outer.dsm.out(0) 

  val io = IO(new Bundle {
    val cmd = Input(new CSRBundle)
    //val busy = Output(Bool())
    val done = Output(Bool())  //interrupt
    val error = Output(Bool()) //interrupt
    val dsmDoneA = Output(Bool()) 
    val nocDoneA = Output(Bool())
    val dsmDoneD = Output(Bool()) 
    val nocDoneD = Output(Bool())
  })

  /* TODO Make sure to update registers only when busy is low! 
   * No asserts provided! */
  val cmd = RegInit({val x = Wire(new CSRBundle);
    x := DontCare; x})
  cmd := io.cmd

  //Mode select: DSM->NoC or NoC->DSM
  //when(cmd.mode) {
    val dst = Wire(new TLBundle(nocEdge.bundle))
    val src = Wire(new TLBundle(dsmEdge.bundle))
  //} .otherwise {
  //  val dst = Wire(new TLBundle(dsmEdge.bundle))
  //  val src = Wire(new TLBundle(nocEdge.bundle))
  //}

  dst.a.ready := Mux(cmd.mode, noc.a.ready, dsm.a.ready) 
  dst.d.bits  := DontCare//Mux(cmd.mode, noc.d.bits , dsm.d.bits ) 
  dst.d.valid := Mux(cmd.mode, noc.d.valid, dsm.d.valid) 
  noc.a.valid := Mux(cmd.mode, dst.a.valid, src.a.valid) 
  noc.a.bits  := DontCare//Mux(cmd.mode, dst.a.bits , src.a.bits ) 
  noc.d.ready := Mux(cmd.mode, dst.d.ready, src.d.ready) 
  dsm.a.valid := Mux(cmd.mode, src.a.valid, dst.a.valid)  
  dsm.a.bits  := DontCare//Mux(cmd.mode, src.a.bits , dst.a.bits )  
  dsm.d.ready := Mux(cmd.mode, src.d.ready, dst.d.ready)  
  src.a.ready := Mux(cmd.mode, dsm.a.ready, noc.a.ready)  
  src.d.bits  := DontCare//Mux(cmd.mode, dsm.d.bits , noc.d.bits )  
  src.d.valid := Mux(cmd.mode, dsm.d.valid, noc.d.valid)
  src.a.bits  := DontCare
  dst.a.bits  := DontCare

  val _dAddr = Module(new AddressGenerator).suggestName("dAddr")
  val _sAddr = Module(new AddressGenerator).suggestName("sAddr")
  val dAddr = _dAddr.io
  val sAddr = _sAddr.io
  dAddr.cmd <> cmd.dest
  sAddr.cmd <> cmd.src
  dAddr.start := cmd.start
  sAddr.start := cmd.start

  val error = WireDefault(Cat(
    noc.d.bits.denied & noc.d.fire(),
    dsm.d.bits.denied & dsm.d.fire()))
  assert(cmd.src.xCnt * cmd.src.yCnt === cmd.dest.xCnt * cmd.dest.yCnt, 
    "Total bytes to be read is not equal to the total bytes to be written!")
  //TODO boundary compliance asserts
  //Agent conflict asserts
  //Address range conflict asserts

  val sAddrDone = RegInit(false.B)
  val lastWrReqSent = RegInit(false.B)
  val done = WireDefault(lastWrReqSent && Mux(cmd.mode, nocEdge.done(noc.d), dsmEdge.done(dsm.d)))
  io.done := done
  when(Mux(cmd.mode, nocEdge.done(noc.a), dsmEdge.done(dsm.a)) && dAddr.out.bits.last) {
    lastWrReqSent := true.B
  } .elsewhen(lastWrReqSent && done) {
    lastWrReqSent := false.B
  }
  when(sAddr.out.bits.last && src.a.fire && ~done) {
    sAddrDone := true.B
  } .elsewhen(sAddrDone && done) {
    sAddrDone := false.B
  }
  //val busy = RegInit(false.B)
  //io.busy := busy
  //when(cmd.start) {
  //  busy := true.B
  //} .elsewhen(done) {
  //  busy := false.B
  //}

  val sIds = RegInit(0.U(4.W))
  val dIds = RegInit(0.U(4.W))
  when(src.a.fire()) { sIds := sIds+1.U }
  io.nocDoneA := nocEdge.done(noc.a)
  io.dsmDoneA := dsmEdge.done(dsm.a)
  io.nocDoneD := nocEdge.done(noc.d)
  io.dsmDoneD := dsmEdge.done(dsm.d)
  when(dst.a.fire() && Mux(cmd.mode, nocEdge.done(noc.a), dsmEdge.done(dsm.a))) {
    dIds := dIds+1.U 
  }

  val q = Module(new Queue(UInt((c.beatBytes*8).W), c.fifoDepth)).suggestName("q")
  q.io.enq.valid := src.d.valid
  q.io.enq.bits := Mux(cmd.mode, dsm.d.bits.data, noc.d.bits.data)
  src.d.ready := q.io.enq.ready
  src.a.valid := cmd.start && sAddr.out.valid && ~sAddrDone
  sAddr.out.ready := src.a.fire() && ~sAddrDone
  /* Note:
   * Mask for Get transactions need to be applied when storing the
   * Get results (from dsm.a) in the mainQueue*/
  //assert(get._1, "Illegal access!")
  q.io.deq.ready := dst.a.fire()
  dst.a.valid := q.io.deq.valid && cmd.start && dAddr.out.valid
  dAddr.out.ready := q.io.deq.valid && Mux(cmd.mode, nocEdge.done(noc.a), dsmEdge.done(dsm.a))
  //assert(put._1, "Illegal access!")
  //dst.a.bits := Mux(cmd.mode, putNoC._2, putDSM._2)
  dst.d.ready := true.B//ignoring everything
  when(cmd.mode) {
    dsm.a.bits := dsmEdge.Get(
      fromSource = sIds, 
      toAddress = sAddr.out.bits.addr,
      lgSize = sAddr.out.bits.size)._2 //TODO mask?
    noc.a.bits := nocEdge.Put(
      fromSource = dIds, 
      toAddress = dAddr.out.bits.addr,
      lgSize = dAddr.out.bits.size,
      data = q.io.deq.bits,
      mask = dAddr.out.bits.mask)._2
  } .otherwise {
    noc.a.bits := nocEdge.Get(
      fromSource = sIds, 
      toAddress = sAddr.out.bits.addr,
      lgSize = sAddr.out.bits.size)._2 //TODO mask?
    dsm.a.bits := dsmEdge.Put(
      fromSource = dIds, 
      toAddress = dAddr.out.bits.addr,
      lgSize = dAddr.out.bits.size,
      data = q.io.deq.bits,
      mask = dAddr.out.bits.mask)._2
  }

  /* Throw an error out based on the error signal in d channel
   * Assuming that we send out valid requests all the time*/
  io.error := error.orR

  /* Holds the latest "Put" transaction status; TODO expose it to IO somehow
   * In case something fails, we could look into this to see what error was seen
   * in the Put port. The Get port error is also similarly logged.*/
  val debugNoC = RegInit(noc.d.bits)
  val debugDSM = RegInit(noc.d.bits)

  /* Problems faced when allowing masked transactions:
   * 1. Start address alignment and row-end-address alignment errors (TL specific)
   * 2. Storing the masked read data in an intermediate queue and then feeding it to the
   *    destination (write) port according to the destinations mask requires complex logic
   *    to select appropriate words from the queue. */

  /* Future work:
   * Between the queue.io and writer, and just before the reader, we could introduce
   * the bus holding logic useful for interleaved txns over a single bus between 
   * both Orch and DMA on one side and DSM on the other side.
   * Currently, we assume that the Orchestrator to DSM reqs and DMA to DSM reqs 
   * pass through an intermediate TLXbar which follows an RR arbitration policy.
   *
   * dAddr should generate addresses and operate independently of sAddr. So once
   * srcAddr is done with its generation, it can move on to the next DMA command
   * while dstAddr is still processing the previous request.
   * However, if the direction of data transfer (DSM <> NoC) changes, then this 
   * would not present any obvious advantage because the put port will be ready 
   * before the queue.io from where the data is sourced is populated. 
   * But considering once the initial configuration data is downloaded from 
   * NoC to DSM, all succeeding transactions will likely be from DSM to NoC, 
   * having this feature implemented would be intermittantly advantageous.*/
}

