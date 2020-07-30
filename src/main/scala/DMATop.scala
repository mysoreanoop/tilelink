package dma

import chisel3._
import chisel3.util._
import chisel3.util.random._

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
    beatBytes: Int = 32,
    dsmSize: BigInt = 0x10000,
    maxBurst: Int = 4,
    fifoDepth: Int = 512,
    nOutstanding: Int = 8,
    maxDMAReqs: Int = 4) {
  //derive from top config?
  val busWidth = beatBytes * 8
  val wordsPerBeat = busWidth/dataWidth
}

case object DMAKey extends Field[DMAParams]//  (new DMAParams)

class WithDMA(in: Boolean) extends Config((site, here, up) => {
  case DMAKey => DMAParams(temp = in)
})

class BaseConfig extends Config(new WithDMA(true))

class PortParam(flag: Boolean) (implicit p: Parameters) extends Bundle {
  val c = p(DMAKey)
  val xStep, yStep = UInt(c.addrCtrlWidth.W)
  val xCnt, yCnt   = UInt(c.addrCtrlWidth.W)
  val addr         = UInt(p(DMAKey).addrWidth.W)
  //val nodeId       = if(flag) Some(UInt(p(DMAKey).idWidth.W)) else None
  val nodeId = UInt(p(DMAKey).idWidth.W)
  
  override def cloneType = (new PortParam(flag)).asInstanceOf[this.type]
}

class CSRBundle(implicit p: Parameters) extends Bundle {
  val src    = new PortParam(true)
  val dest   = new PortParam(true)
  val txnId  = UInt(log2Up(p(DMAKey).maxDMAReqs).W)

  override def cloneType = (new CSRBundle).asInstanceOf[this.type]
}

class DMA(implicit p: Parameters) extends LazyModule {
  val size = p(DMAKey).dsmSize
//  val noc = TLHelper.makeClientNode(TLMasterParameters.v1(
//    name = "dmaSlaveToNoC",
//    sourceId = IdRange(0, 128),
//    requestFifo = true,
//    visibility = Seq(AddressSet(0x10000, 0xffff))))

  val dsm = TLHelper.makeClientNode(TLMasterParameters.v1(
    name = "dmaSlaveToDSM",
    sourceId = IdRange(0, 512),
    requestFifo = true,
    visibility = Seq(AddressSet(0x0, 0xffff))))
  
  lazy val module = new DMAModule(this)
}

class DMAModule(outer: DMA) extends LazyModuleImp(outer) {
  val c = p(DMAKey)
//  val (noc, nocEdge) = outer.noc.out(0)
  val (dsm, dsmEdge) = outer.dsm.out(0) 
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CSRBundle))
    val status = Valid(UInt(log2Up(p(DMAKey).maxDMAReqs).W))
    val error = Output(Bool())
    //val dsmIO = new TLBundle(dsmEdge.bundle)
  //  val nocIO = new TLBundle(nocEdge.bundle)
  //  val op0 = Decoupled(UInt(32.W))
    val op1 = Decoupled(UInt(32.W))
  })
  
  val cmd = Queue(io.in,  p(DMAKey).nOutstanding) //pipe, flow TODO
  cmd.ready := true.B
//  io.dsmIO <> dsm
//  io.nocIO <> noc
//  val sAddr = Module(new AddressGenerator).io
//  val dAddr = Module(new AddressGenerator).io
//  sAddr.cmd.valid := cmd.valid
//  sAddr.cmd.bits := cmd.bits.src
//  dAddr.cmd.valid := cmd.valid
//  dAddr.cmd.bits := cmd.bits.dest

  val dIds = RegInit(0.U(16.W))
//  val nIds = RegInit(320.U(10.W))
   dIds := dIds + 1.U 
//   nIds := nIds + 1.U 
////
  val queue = Module(new Queue(UInt(32.W), 5, true, true)).io
//  val queue1 = Module(new Queue(UInt(32.W), 5, true, true)).io
//  queue1.enq.valid := false.B
//  queue1.deq.ready := false.B
  queue.enq.valid := false.B
  queue.deq.ready := false.B
//  io.op0.valid := noc.d.valid
//  io.op0.bits := noc.d.bits.data
  io.op1.valid := dsm.d.valid
  io.op1.bits := dsm.d.bits.data
  
//  queue1.enq.bits := noc.d.bits.data
  queue.enq.bits := dsm.d.bits.data
  val d = LFSR(16)
  val q = RegInit(0.U(16.W))
  val t = RegInit(0.U(8.W))
  q := q+1.U
//  noc.a.valid := false.B
//  noc.d.ready := false.B
//  noc.a.bits := DontCare
  dsm.a.valid := false.B
  dsm.d.ready := true.B
  dsm.a.bits := DontCare
  io.error := dsm.d.bits.denied
  when(q>100.U && q<200.U) {
//    noc.a.valid := true.B
//    noc.a.bits := nocEdge.Put(fromSource = nIds, 
//      toAddress = "h10000".U + t*32.U, lgSize = 1.U,
//      data = d)._2
//    noc.d.ready := true.B

    dsm.a.valid := true.B
    dsm.a.bits := dsmEdge.Put(fromSource = dIds, 
      toAddress =t*32.U, lgSize = 1.U,
      data = d+"h10".U(32.W), mask = "h3".U)._2
    dsm.d.ready := true.B
    t := t+1.U
    when(q === 199.U) {
      t := 0.U
    }
  } .elsewhen(q>200.U) {
//    noc.a.valid := true.B
//    noc.a.bits := nocEdge.Get(fromSource = nIds, 
//      toAddress = "h10000".U + t*32.U, lgSize = 1.U)._2
//    noc.d.ready := true.B

    queue.enq.valid := dsm.d.valid
    dsm.d.ready := queue.enq.ready

    when(q>205.U) {queue.deq.ready := true.B}

    dsm.a.valid := true.B
    dsm.a.bits := dsmEdge.Get(fromSource = dIds, 
      toAddress = t*32.U, lgSize = 1.U)._2

//    queue1.enq.valid := noc.d.valid
//    noc.d.ready := queue1.enq.ready
    t := t+1.U
  }
////

//  val queue = Module(new Queue(UInt(c.busWidth.W), c.fifoDepth, true, true)).io
//  val get = dsmEdge.Get(fromSource = dIds, //cmd.bits.src.nodeId, 
//    toAddress = sAddr.out.addr, lgSize = 1.U)._2//sAddr.out.len)._2//TODO sAddr.mask?
//    //dsmEdge.get -> nocEdge.get
//  val put = nocEdge.Put(fromSource = nIds, //cmd.bits.dest.nodeId, 
//    toAddress = dAddr.out.addr, lgSize = 1.U,// dAddr.out.len, 
//    data = queue.deq.bits)._2//, mask = dAddr.out.mask)._2
//
////  when(cmd.bits.src.nodeId === 0.U) {
//    dsm.a.valid := cmd.valid
//    dsm.a.bits := get
//    sAddr.req := dsm.a.ready
//    queue.enq.valid := dsm.d.valid
//    queue.enq.bits  := dsm.d.bits.data
//    dsm.d.ready := queue.enq.ready
//
//    noc.a.valid := queue.deq.valid && cmd.valid
//    noc.a.bits := put
//    queue.deq.ready := noc.a.ready
//    dAddr.req := queue.deq.valid && noc.a.ready
//    noc.d.ready := true.B//always ready for write response
//  } .otherwise {
//    noc.a.valid := cmd.valid
//    noc.a.bits := get
//    sAddr.req := noc.a.ready
//    queue.enq.valid := noc.d.valid
//    queue.enq.bits := noc.d.bits.data
//    noc.d.ready := queue.enq.ready
//
//    dsm.a.valid := queue.deq.valid && cmd.valid
//    dsm.a.bits := put
//    queue.deq.ready := dsm.a.ready
//    dAddr.req := queue.deq.valid && dsm.a.ready
//    dsm.d.ready := true.B
//  }
  
  //cmd.ready := dAddr.last
  io.status.bits := 0.U//cmd.bits.txnId
  io.status.valid := false.B//dAddr.last

  //Throw an error out based on the error signal in d channel
  //io.error := false.B//Mux(cmd.bits.src.nodeId === 0.U, noc.d.bits.denied, dsm.d.bits.denied)
  //Assuming that we send out valid requests all the time.

  /* Holds the latest put transaction status; TODO expose it to IO somehow
   * In case something fails, we could look into this to see what error was seen
   * in the put port. The get port error is also similarly logged.*/
  //val debugResp = RegInit(Mux(cmd.bits.src.nodeId === 0.U, noc.d.bits, dsm.d.bits))
  //val debugReq = RegInit(Mux(cmd.bits.src.nodeId === 0.U, dsm.d.bits, noc.d.bits))
  /* Future work:
   * Between the queue and writer, and just before the reader, we could introduce
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
   * before the queue from where the data is sourced is populated. 
   * But considering once the initial configuration data is downloaded from 
   * NoC to DSM, all succeeding transactions will likely be from DSM to NoC, 
   * having this feature implemented would be sporadically advantageous.*/
}

/* Produces address to read from or write to.
 * To be used in the right context depending on which agent is the source.
 */
class AddressGenerator(implicit p: Parameters) extends Module {
  val c = p(DMAKey)
  val io = IO(new Bundle {
    val out  = Output(new Bundle{
      val addr = UInt(p(DMAKey).addrWidth.W)
      val len = UInt(log2Up(p(DMAKey).maxBurst).W)
      val mask = UInt(p(DMAKey).wordsPerBeat.W)
    })
    val cmd   = Flipped(Valid(new PortParam(false)))
    val last  = Output(Bool())
    val req   = Input(Bool())
  })

  val cmd = RegInit({val n = Wire(new PortParam(false));
    n := DontCare; n})
  val mask = RegInit(0.U(c.wordsPerBeat.W))
  val len  = RegInit((c.maxBurst-1).U(log2Up(c.maxBurst).W))

  def calculate(n:PortParam) = {
    val out = Wire(new PortParam(false))
    out := n
    when(n.xCnt > 0.U) {
      /*This exploits busWidth and burst advantages.*/
      when(n.xCnt >= (c.wordsPerBeat * c.maxBurst).U) {
        out.xCnt := n.xCnt - (c.wordsPerBeat * c.maxBurst).U
        mask := Fill(c.wordsPerBeat, 1.U)
        len := c.maxBurst.U
        out.addr := n.addr + (c.wordsPerBeat * c.maxBurst).U * (c.dataWidth/8).U 
        //Assuming xStep = 1
      } .otherwise {
        /* Cut it up into as many single beats and one final masked beat
         * Determining max possible burstSize is costly*/
        len := 1.U
        when(n.xCnt >= c.wordsPerBeat.U) {
          mask := Fill(c.wordsPerBeat, 1.U)
          out.xCnt := n.xCnt - c.wordsPerBeat.U
          out.addr := n.addr + c.wordsPerBeat.U * (c.dataWidth/8).U
          //if xCnt becomes zero, then it goes to the xCnt===0 loop, which is fine
        } .otherwise {//masked beat
          mask := ((1.U << n.xCnt) - 1.U)// << (c.wordsPerBeat.U - n.xCnt)
          out.xCnt := 0.U
          out.addr := n.addr + n.xCnt * (c.dataWidth/8).U
        }
      }
    } .otherwise {//.elsewhen(n.xCnt === 0.U) {
      when(n.yCnt > 0.U) {
        out.xCnt := io.cmd.bits.xCnt
        out.addr := n.addr + n.yStep * (c.dataWidth/8).U
        //yStep and yCnt can be any value
        out.yCnt := n.yCnt - 1.U
      } .otherwise {//Done
        io.last := true.B
      }
    }
    out
  }
  
  //io.inUse    := (cmd.yCnt =/= 0.U) || (cmd.xCnt =/= 0.U)
  io.last := false.B
  val cmdV = RegNext(io.cmd.valid) 
  io.out.addr := cmd.addr //TODO misses the first addr!
  io.out.len := len
  io.out.mask := mask
  when(io.req && ~io.cmd.valid) {
    cmd := calculate(cmd)
  } .elsewhen(io.cmd.valid) {
    cmd := Mux(io.req, calculate(io.cmd.bits), io.cmd.bits)
    assert(io.cmd.bits.xStep === 1.U, 
      "Address increment after every burst != 1;" +
      "This leads to a very inefficient mode of data transfer!")
  }
}

class vec4 extends Bundle {
  val out = Vec(4, UInt(8.W))
}
class mask extends Bundle {
  val out = Vec(4, Bool())
}
class ManagerTL(implicit p: Parameters) extends LazyModule {
  val device = new SimpleDevice("ManagerTL", Seq())
  val beatBytes = 4
  val node = TLHelper.makeManagerNode(beatBytes, TLSlaveParameters.v1(
    address = Seq(AddressSet(0x0, 0xffff)),
    resources = device.reg,
    regionType = RegionType.UNCACHED,
    executable = true,
    supportsGet = TransferSizes(1, beatBytes),
    supportsPutFull = TransferSizes(1, beatBytes),
    supportsPutPartial = TransferSizes(1, beatBytes)))
  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.in(0)
    val mem = Mem(1024, Vec(4, UInt(8.W)))
    val c = RegInit(0.U(8.W))
    c := c + 1.U
    when(c<100.U) {
      mem.write(c*4.U, VecInit(Seq.fill(4)(c)), VecInit(Seq.fill(4)(true.B)))
    }

    tl.a.ready := true.B
    tl.d.valid := false.B
    tl.d.bits := DontCare
    when(tl.a.fire() && tl.a.bits.opcode === 0.U) {
      mem.write(tl.a.bits.address, 
        tl.a.bits.data.asTypeOf(new vec4).out, 
        tl.a.bits.mask.asTypeOf(new mask).out)
      tl.d.valid := true.B
      tl.d.bits := edge.AccessAck(tl.a.bits)
    } .elsewhen(tl.a.fire() && tl.a.bits.opcode === 4.U) {
      tl.d.bits := edge.AccessAck(tl.a.bits, mem.read(tl.a.bits.address).asUInt)
      tl.d.valid := true.B
    }
  }
}

class CRTemp(implicit p: Parameters) extends LazyModule {
  //val ram1 = LazyModule(new TLRAM(address=AddressSet(0x10000, 0xffff), 
  //  beatBytes=4))
  //val ram0 = LazyModule(new ManagerTL)
  val ram0 = LazyModule(new TLRAM(address=AddressSet(0, 0xffff), 
    beatBytes=4))
  //TODO check conflicting addressSets
  val dma  = LazyModule(new DMA)
  ram0.node := dma.dsm
  //ram1.node := dma.noc

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val other = dma.module.io.cloneType
      //val dsm = dma.module.io.dsmIO.cloneType
      //val noc = dma.module.io.nocIO.cloneType
    })
    //io.dsm <> dma.module.io.dsmIO
    //io.noc <> dma.module.io.nocIO
    io.other <> dma.module.io
  }
}
