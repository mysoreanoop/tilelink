package dma

import chisel3._
import chisel3.util._

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
    dsmSize: BigInt = 0x100000,
    maxBurst: Int = 64,
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
  val nodeId       = if(flag) Some(UInt(p(DMAKey).idWidth.W)) else None
  
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
  val noc = TLHelper.makeClientNode(TLMasterParameters.v1(
    name = "dmaSlaveToNoC",
    sourceId = IdRange(0, 128),
    requestFifo = true,
    visibility = Seq(AddressSet(0x0, 0xffff))))

  val dsm = TLHelper.makeClientNode(TLMasterParameters.v1(
    name = "dmaSlaveToDSM",
    sourceId = IdRange(0,4),
    requestFifo = true,
    visibility = Seq(AddressSet(0x0, size-1))))
  
  lazy val module = new DMAModule(this)
}

class DMAModule(outer: DMA) extends LazyModuleImp(outer) {
  val c = p(DMAKey)
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CSRBundle))
    val status = Valid(UInt(log2Up(p(DMAKey).maxDMAReqs).W))
  })
  
  val (noc, nocEdge) = outer.noc.out(0)
  val (dsm, dsmEdge) = outer.dsm.out(0) 
  val cmd = Queue(io.in,  p(DMAKey).nOutstanding) //pipe, flow TODO

  val sAddr = Module(new AddressGenerator).io
  val dAddr = Module(new AddressGenerator).io
  sAddr.cmd.valid := cmd.valid
  sAddr.cmd.bits := cmd.bits.src
  dAddr.cmd.valid := cmd.valid
  dAddr.cmd.bits := cmd.bits.dest

  val queue = Module(new Queue(UInt(c.busWidth.W), c.fifoDepth, true, true)).io
//  when(cmd.bits.src.nodeId.get === 0.U) {
    queue.enq.valid := dsm.d.valid
    queue.enq.bits  := dsm.d.bits.data
    dsm.d.ready := queue.enq.ready
//  } .otherwise {
//    queue.enq.valid := noc.d.valid
//    queue.enq.bits  := noc.d.bits.data
//    noc.d.ready := queue.enq.ready
//  }
  //TODO queue.deq 

  val get = dsmEdge.Get(fromSource = cmd.bits.src.nodeId.get, 
    toAddress = sAddr.out.addr, lgSize = sAddr.out.len)._2//TODO sAddr.mask?
  val put = nocEdge.Put(fromSource = cmd.bits.dest.nodeId.get, 
    toAddress = dAddr.out.addr, lgSize = dAddr.out.len, 
    data = queue.deq.bits, mask = dAddr.out.mask)._2

//  when(cmd.bits.src.nodeId.get === 0.U) {
    dsm.a.valid := cmd.valid
    dsm.a.bits := get
    sAddr.req := dsm.a.ready
    queue.enq.valid := dsm.d.valid
    dsm.d.ready := queue.enq.ready

    noc.a.valid := queue.deq.valid && cmd.valid
    noc.a.bits := put
    queue.deq.ready := noc.a.ready
    dAddr.req := noc.a.ready
//  } .otherwise {
//    noc.a.valid := cmd.valid
//    noc.a.bits := get
//    sAddr.req := noc.a.ready
//    queue.enq.valid := noc.d.valid
//    noc.d.ready := queue.enq.ready
//
//    dsm.a.valid := queue.deq.valid && cmd.valid
//    dsm.a.bits := put
//    queue.deq.ready := dsm.a.ready
//    dAddr.req := dsm.a.ready
//  }
  
  cmd.ready := dAddr.last
  io.status.bits := cmd.bits.txnId
  io.status.valid := dAddr.last

  /* Holds the latest put transaction status;
   * In case something fails, we could look into this.*/
  val debugResp = RegInit(Mux(cmd.bits.src.nodeId.get === 0.U, dsm.d.bits, noc.d.bits))

  /* Future work:
   * Between the queue and writer, and just before the reader, we could introduce
   * the bus holding logic useful for interleaved txns over a single bus between 
   * both Orch and DMA on one side and DSM on the other side.
   * Currently, we assume that the Orchestrator to DSM reqs and DMA to DSM reqs 
   * pass through an intermediate TLXbar which follows an RR arbitration policy.
   *
   * dAddr should generate addresses and operate independently of sAddr. So once
   * srcAddr is done with its generation, it can move on to the next DMA command
   * while dstAddr is still processing the previous request because of bus clash.*/
  
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
  io.out.addr := Mux(io.cmd.valid, io.cmd.bits.addr, cmd.addr)
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

class CRTemp(implicit p: Parameters) extends LazyModule {
  val ram0 = LazyModule(new TLRAM(address=AddressSet(0, 0xffff)))
  val ram1 = LazyModule(new TLRAM(address=AddressSet(0, 0xffff)))
  val dma  = LazyModule(new DMA)
  ram0.node := dma.noc
  ram1.node := dma.dsm

  lazy val module = new LazyModuleImp(this) {
    val io = IO(dma.module.io.cloneType)
    io <> dma.module.io
  }
}
