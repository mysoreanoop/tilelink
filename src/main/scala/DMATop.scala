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

case class DSMParams(
    temp: Boolean, 
    dsmSize: BigInt = 0x10000,
    nPorts: Int = 8,
    dataWidth: Int = 32, 
    addrWidth: Int = 32) {
  require(dsmSize%nPorts==0, "DSM size has to be in multiples of port size")
}

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
case object DMAKey extends Field[DMAParams]//  (new DMAParams)

class WithDSM(in:Boolean) extends Config((site, here, up) => {
  case DSMKey => DSMParams(temp = in)
})
class WithDMA(in: Boolean) extends Config((site, here, up) => {
  case DMAKey => DMAParams(temp = in)
})

class BaseConfig extends Config(new WithDMA(true) ++ new WithDSM(true))

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
  val txnId  = UInt(p(DMAKey).txnIdWidth.W)

  override def cloneType = (new CSRBundle).asInstanceOf[this.type]
}

class DMA(implicit p: Parameters) extends LazyModule {
  val size = p(DSMKey).dsmSize
//  val noc = TLHelper.makeClientNode(TLMasterParameters.v1(
//    name = "dmaSlaveToNoC",
//    sourceId = IdRange(0, 16),
//    requestFifo = true,
//    visibility = Seq(AddressSet(0x0, 0xffffff))))

  val dsm = TLHelper.makeClientNode(TLMasterParameters.v1(
    name = "dmaSlaveToDSM",
    sourceId = IdRange(0, 16),
    requestFifo = true,
    visibility = Seq(AddressSet(0x0, 0xffffff))))
  
  lazy val module = new DMAModule(this)
}

class DMAModule(outer: DMA) extends LazyModuleImp(outer) {
  val c = p(DMAKey)
//  val (noc, nocEdge) = outer.noc.out(0)
  val (dsm, dsmEdge) = outer.dsm.out(0) 

//  val dAddr = Module(new AddressGenerator).io
  val sAddr = Module(new AddressGenerator).io

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CSRBundle))
    //val status = Decoupled(UInt(c.txnIdWidth.W))
    val error = Output(Bool())
//    val dOut = dAddr.out.cloneType
//    val sOut = sAddr.out.cloneType
    val qOut = Decoupled(UInt(256.W))
  })
  val h = io.in.bits
  assert(h.src.xCnt * h.src.yCnt === h.dest.xCnt * h.dest.yCnt, 
    "Total bytes to be read is not equal to the total bytes to be written!")

//  io.dOut <> dAddr.out
//  io.sOut <> sAddr.out

  val sCmd = Module(new Queue(new PortParam(true), c.nOutstanding, true, true)).io
//val dCmd = Module(new Queue(new PortParam(true), c.nOutstanding, true, true)).io
//  val txn = Module(new Queue(UInt(c.txnIdWidth.W), c.nOutstanding, true, true)).io
  io.in.ready := sCmd.enq.ready// & txn.enq.ready //& dCmd.enq.ready
  sCmd.enq.valid := io.in.valid
  //dCmd.enq.valid := io.in.valid
//  txn.enq.valid := io.in.valid
  sCmd.enq.bits := io.in.bits.src
  //dCmd.enq.bits := io.in.bits.dest
//  txn.enq.bits := io.in.bits.txnId

  //dAddr.cmd <> dCmd.deq
  //dAddr.out.ready := false.B
  sAddr.cmd <> sCmd.deq
  //sAddr.out.ready := false.B
  //txn.deq <> io.status

  val sIds = RegInit(0.U(4.W))
  //val dIds = RegInit(0.U(4.W))
 
  val queue = Module(new Queue(UInt((c.beatBytes*8).W), c.fifoDepth, true, true)).suggestName("mainQueue")
  queue.io.enq.valid := dsm.d.valid
  queue.io.enq.bits := dsm.d.bits.data
  dsm.d.ready := queue.io.enq.ready
  //queue.io.deq.ready := false.B
  queue.io.deq <> io.qOut

  //queue.io.deq.ready := noc.a.ready
  //noc.a.valid := queue.io.deq.valid
  val next = RegInit(false.B)
  when(dsm.a.fire()) {
      //sAddr.out.bits.last && sAddr.out.valid) {
    sIds := sIds+1.U
    //sAddr.out.ready := true.B
  }
//  when(noc.a.fire() && nocEdge.last(noc.a)) {
//    dIds := dIds+1.U
//    dAddr.out.ready := true.B
//  }
  dsm.a.valid := sAddr.out.valid
  sAddr.out.ready := dsm.a.ready
  dsm.a.bits := dsmEdge.Get(
    fromSource = sIds, 
    toAddress = sAddr.out.bits.addr,
    lgSize = sAddr.out.bits.size)._2 //TODO ._1
  //TODO what to do with mask?

//  noc.a.valid := true.B
//  dAddr.out.ready := dCmd.deq.valid && ~RegInit(dCmd.deq.valid)
//  noc.a.bits := nocEdge.Put(
//    fromSource = dIds, 
//    toAddress = dAddr.out.bits.addr,
//    lgSize = dAddr.out.bits.size,
//    data = queue.io.deq.bits,
//    mask = dAddr.out.bits.mask)._2
////

//  val queue.io = Module(new Queue(UInt(c.busWidth.W), c.fifoDepth, true, true)).io
//  val get = dsmEdge.Get(fromSource = dIds, //cmd.bits.src.nodeId, 
//    toAddress = sAddr.out.addr, lgSize = 1.U)._2//sAddr.out.size)._2//TODO sAddr.mask?
//    //dsmEdge.get -> nocEdge.get
//  val put = nocEdge.Put(fromSource = nIds, //cmd.bits.dest.nodeId, 
//    toAddress = dAddr.out.addr, lgSize = 1.U,// dAddr.out.size, 
//    data = queue.io.deq.bits)._2//, mask = dAddr.out.mask)._2
//
////  when(cmd.bits.src.nodeId === 0.U) {
//    dsm.a.valid := cmd.valid
//    dsm.a.bits := get
//    sAddr.req := dsm.a.ready
//    queue.io.enq.valid := dsm.d.valid
//    queue.io.enq.bits  := dsm.d.bits.data
//    dsm.d.ready := queue.io.enq.ready
//
//    noc.a.valid := queue.io.deq.valid && cmd.valid
//    noc.a.bits := put
//    queue.io.deq.ready := noc.a.ready
//    dAddr.req := queue.io.deq.valid && noc.a.ready
//    noc.d.ready := true.B//always ready for write response
//  } .otherwise {
//    noc.a.valid := cmd.valid
//    noc.a.bits := get
//    sAddr.req := noc.a.ready
//    queue.io.enq.valid := noc.d.valid
//    queue.io.enq.bits := noc.d.bits.data
//    noc.d.ready := queue.io.enq.ready
//
//    dsm.a.valid := queue.io.deq.valid && cmd.valid
//    dsm.a.bits := put
//    queue.io.deq.ready := dsm.a.ready
//    dAddr.req := queue.io.deq.valid && dsm.a.ready
//    dsm.d.ready := true.B
//  }
  
  io.error := dsm.d.bits.denied
//  io.status.bits := 0.U//cmd.bits.txnId
//  io.status.valid := false.B//dAddr.last

  //Throw an error out based on the error signal in d channel
  //io.error := false.B//Mux(cmd.bits.src.nodeId === 0.U, noc.d.bits.denied, dsm.d.bits.denied)
  //Assuming that we send out valid requests all the time.

  /* Holds the latest put transaction status; TODO expose it to IO somehow
   * In case something fails, we could look into this to see what error was seen
   * in the put port. The get port error is also similarly logged.*/
  //val debugResp = RegInit(Mux(cmd.bits.src.nodeId === 0.U, noc.d.bits, dsm.d.bits))
  //val debugReq = RegInit(Mux(cmd.bits.src.nodeId === 0.U, dsm.d.bits, noc.d.bits))
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
   * having this feature implemented would be sporadically advantageous.*/
}
class AddrOut(implicit p: Parameters) extends Bundle {
  val c = p(DMAKey)
  val addr = UInt(c.addrWidth.W)
  val size = UInt(log2Up(c.lgMaxBytes).W)
  val mask = UInt(c.beatBytes.W)
  val last = Bool()

  override def cloneType = (new AddrOut).asInstanceOf[this.type]
}

/* Produces address to read from or write to*/
class AddressGenerator(implicit p: Parameters) extends Module {
  val c = p(DMAKey)
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new PortParam(false)))
    val out = Decoupled(new AddrOut)
  })
  val cmd = RegInit({val n = Wire(new PortParam(false));//xStep and yStep need not be reg'd
    n := DontCare; n})
  val last = calcLast(cmd.xCnt, cmd.yCnt) & io.cmd.valid
  val fresh = io.cmd.valid & (RegNext(last) | ~RegNext(io.cmd.valid))

  io.cmd.ready := last & io.out.ready
  io.out.valid := io.cmd.valid
  io.out.bits.last := last 
  io.out.bits.addr := cmd.addr 
  io.out.bits.size := calcSize(cmd.xCnt)
  io.out.bits.mask := calcMask(cmd.xCnt)
  when(fresh) {
    io.out.valid := false.B
  //  when(io.out.ready) { //TODO causes combinational loop
      //io.out.bits.addr := io.cmd.bits.addr
      //io.out.bits.size := calcSize(io.cmd.bits.xCnt)
      //io.out.bits.mask := calcMask(io.cmd.bits.xCnt)
      //io.out.bits.last := calcLast(io.cmd.bits.xCnt, io.cmd.bits.yCnt)
      //cmd := calcAddr(io.cmd.bits)
      //assert() TODO
  //  } .otherwise {
      cmd  := io.cmd.bits
  //  }
  } .otherwise {
    when(io.out.ready) {
      cmd  := calcAddr(cmd)
    }
  }

  def calcSize(x: UInt): UInt = {
    val out = Wire(UInt(c.maxWords.W))
    when(x >= c.maxWords.U) {
      out := c.lgMaxBytes.U
    } .otherwise {
      out := 0.U//means 1 beat
    }
    out
  }
  def calcMask(x: UInt): UInt = {
    def createMask(n: UInt): UInt = {
      //Reverse((1.U << n) - 1.U) TODO hangs!!!
      (1.U << n) - 1.U 
    }
    val out = Wire(UInt(c.beatBytes.W))
    when(x >= c.wordsPerBeat.U) {
      out := Fill(c.beatBytes, 1.U)
    } .otherwise {
      out := createMask(x)
      //^for the remaining words that cannot make up an entire beat
    }
    out
  }
  def calcLast(x: UInt, y: UInt): Bool = {
    val out = WireInit(false.B)
    when(y === 0.U) {
      when(x === c.maxWords.U || x <= c.wordsPerBeat.U) {
        out := true.B
      }
    }
    out
  }
  def calcAddr(n:PortParam) = {
    val out = Wire(new PortParam(false))
    out := n
    when(n.xCnt > 0.U) {
      /*This exploits busWidth and burst advantages.*/
      when(n.xCnt >= c.maxWords.U) {
        when(n.xCnt === c.maxWords.U) {
          out.xCnt := Mux(n.yCnt > 0.U, io.cmd.bits.xCnt, 0.U)
          out.yCnt := n.yCnt - 1.U //will be -ve during last
          out.addr := n.addr + c.maxBytes.U + n.yStep * (c.dataWidth/8).U
        } .otherwise {
          out.xCnt := n.xCnt - c.maxWords.U
          out.addr := n.addr + c.maxBytes.U
        }
      } .otherwise {
        /* Cut it up into as many single beats and one final masked beat
         * Determining single max possible burstSize is costly*/
        when(n.xCnt >= c.wordsPerBeat.U) {
          when(n.xCnt === c.wordsPerBeat.U) {
            out.xCnt := Mux(n.yCnt > 0.U, io.cmd.bits.xCnt, 0.U)
            out.addr := n.addr + c.beatBytes.U + n.yStep * (c.dataWidth/8).U
            out.yCnt := n.yCnt - 1.U
          } .otherwise {
            out.xCnt := n.xCnt - c.wordsPerBeat.U
            out.addr := n.addr + c.beatBytes.U
          }
        } .otherwise {//masked beat
          out.xCnt := Mux(n.yCnt > 0.U, io.cmd.bits.xCnt, 0.U)
          out.addr := n.addr + n.xCnt * (c.dataWidth/8).U
          out.yCnt := n.yCnt - 1.U
        }
      }
    } .otherwise {
      //don't come here!
    }
    out
  }
}
// Given an address and size, create a mask of beatBytes size
// eg: (0x3, 0, 4) => 0001, (0x3, 1, 4) => 0011, (0x3, 2, 4) => 1111
// groupBy applies an interleaved OR reduction; groupBy=2 take 0010 => 01
//object MaskGen {
//  def apply(addr_lo: UInt, lgSize: UInt, beatBytes: Int, groupBy: Int = 1): UInt = { 
//    require (groupBy >= 1 && beatBytes >= groupBy)
//    require (isPow2(beatBytes) && isPow2(groupBy))
//    val lgBytes = log2Ceil(beatBytes)
//    val sizeOH = UIntToOH(lgSize | 0.U(log2Up(beatBytes).W), log2Up(beatBytes)) | UInt(groupBy*2 - 1)
//
//    def helper(i: Int): Seq[(Bool, Bool)] = { 
//      if (i == 0) {
//        Seq((lgSize >= UInt(lgBytes), Bool(true)))
//      } else {
//        val sub = helper(i-1)
//        val size = sizeOH(lgBytes - i)
//        val bit = addr_lo(lgBytes - i)
//        val nbit = !bit
//        Seq.tabulate (1 << i) { j =>
//          val (sub_acc, sub_eq) = sub(j/2)
//          val eq = sub_eq && (if (j % 2 == 1) bit else nbit)
//          val acc = sub_acc || (size && eq)
//          (acc, eq)
//        }
//      }
//    }
//
//    if (groupBy == beatBytes) UInt(1) else
//      Cat(helper(lgBytes-log2Ceil(groupBy)).map(_._1).reverse)
//  }
//}
//
//class ManagerTL(implicit p: Parameters) extends LazyModule {
//  val device = new SimpleDevice("ManagerTL", Seq())
//  val beatBytes = 32
//  val node = TLHelper.makeManagerNode(beatBytes, TLSlaveParameters.v1(
//    address = Seq(AddressSet(0x0, 0xffff)),
//    resources = device.reg,
//    regionType = RegionType.UNCACHED,
//    executable = true,
//    supportsGet = TransferSizes(1, beatBytes),
//    supportsPutFull = TransferSizes(1, beatBytes),
//    supportsPutPartial = TransferSizes(1, beatBytes)))
//  lazy val module = new LazyModuleImp(this) {
//    val (tl, edge) = node.in(0)
//    val mem = Module(new mkDataMem).io
//    mem.wrAddr.valid := false.B
//    mem.wrAddr.bits := DontCare
//    mem.wrData := DontCare
//    mem.strobe := DontCare
//    mem.rdAddr.valid := false.B
//    mem.rdAddr.bits := DontCare
//    
//    def groupBy4(in: UInt): Vec[Bool] = {
//      val out = RegInit(VecInit(Seq.fill(8) (false.B)))
//      for(i <- 0 until in.getWidth/4) {
//        out(i) := in(i*4+3, i*4)
//      }
//      out
//    }
//
//    val aReady = RegInit(false.B)
//    tl.a.ready := aReady
//    val dValid = RegInit(false.B)
//    val bytes = RegInit(0.U(8.W))
//    tl.d.valid := dValid
//    tl.d.bits := edge.AccessAck(tl.a.bits)
//    //XXX Completely ignoring mask here!
//    when(tl.a.valid) {
//      //assert(tl.a.bits.size < 2.U, "Reading less than a word or nothing!")
//      assert(tl.a.bits.address(1,0) === 0.U, "Address not aligned to word boundary!")
//      when(tl.a.bits.opcode === 0.U || tl.a.bits.opcode === 1.U) {
//        when(tl.a.bits.size <= 32.U) {
//          //Single beat (with strobe)
//          assert(bytes === 0.U, "Last burst txn was abandoned midway!")
//          mem.wrAddr.valid := true.B
//          mem.wrAddr.bits := tl.a.bits.address(15, 5)
//          mem.wrData := tl.a.bits.data.asTypeOf(Vec(8, UInt(32.W)))
//          //mem.strobe := Mux(tl.a.bits.size === 5.U, Fill(8, 1.U), 
//          //  //MaskGen(tl.a.bits.addr, tl.a.bits.size, 32, 4))
//          //  ((1 << tl.bits.size)-1 << tl.a.bits.addr(4,2)))
//          //^ cannot assert a condition here, so writer be safe!
//          mem.strobe := groupBy4(tl.a.bits.mask)
//          tl.a.ready := true.B
//          dValid := true.B
//        } .otherwise {
//          //Multi beat
//          dValid := false.B
//          bytes := Mux(bytes === 0.U, tl.a.bits.size - 32.U, 
//            Mux(bytes < 32.U, 0.U, bytes - 32.U))
//          mem.wrAddr.valid := true.B
//          mem.wrAddr.bits := Mux(bytes === 0.U, tl.a.bits.address(15,5), 
//            tl.a.bits.address(15,5) + 32.U * bytes)
//          mem.wrData := tl.a.bits.data.asTypeOf(Vec(8, UInt(32.W)))
//          mem.strobe := groupBy4(tl.a.bits.mask)
//          tl.a.ready := true.B
//          dValid := bytes <= 32.U
//        }
//      } .elsewhen(tl.a.bits.opcode === 4.U) {
//        //Mem read operations
//        tl.a.ready := true.B
//        aReady := false.B
//        when(tl.a.bits.size <= 3.U) {
//          dValid := true.B
//        }
//        
//      }
//    }
//  }
//}

class CRTemp(implicit p: Parameters) extends LazyModule {
  val rom = LazyModule(new TLROM(
    base = 0,
    size = 0x10000,
    contentsDelayed = Seq.tabulate(0x10000) {i=>i.toByte},
    beatBytes = 32))
  //val ram0 = LazyModule(new ManagerTL)
  //val rom = LazyModule(new TLRAM(address=AddressSet(0, 0xffffff), 
  //  beatBytes = 32))
//  val ram = LazyModule(new TLRAM(address=AddressSet(0, 0xffffff), 
//    beatBytes = 32))

  val dma  = LazyModule(new DMA)
  rom.node := TLFragmenter(32,p(DMAKey).maxBurst * 32) := dma.dsm
//  ram.node := TLFragmenter(32,p(DMAKey).maxBurst * 32) := dma.noc

  lazy val module = new LazyModuleImp(this) {
    val io = IO(dma.module.io.cloneType)
    io <> dma.module.io
  }
}

//memories
/* Data storage: Simple Dual-Port, Synchronous Write & Synhcronous read
*  Organization: 8 * 4096 x 32-bit module
*  Addressing format:
*   Port select     : offset[4:2]
*   Addr within port: {index, way, offset[9:5]}
* Note: If both read and write are active at the time for the same addr, 
* read is prioritized; For the write to complete, 
* stop reading from the competing address.
*/

//class mkDataMem(implicit p: Parameters) extends Module {
//  val c = p(DSMKey)
//  val io = IO(new Bundle{
//    val rdAddr = Flipped(Decoupled(UInt(c.addrWidth.W)))
//    val rdData = Vec(c.nPorts, Output(UInt(c.dataWidth.W)))
//    val wrAddr = Flipped(Decoupled(UInt(c.addrWidth.W)))
//    val wrData = Vec(c.nPorts, Input(UInt(c.dataWidth.W)))
//    val strobe = Vec(c.nPorts, Input(Bool()))
//  })
//
//  val storage = Seq.fill(c.nPorts) {SyncReadMem(c.dsmSize/c.nPorts, UInt(c.dataWidth.W))}
//  val rdData = Seq.fill(c.nPorts) {RegInit(0.U(c.dataWidth.W))}
//  io.rdAddr.ready := true.B
//  when(io.rdAddr.valid) {
//    for(i <- 0 until c.nPorts) {
//        rdData(i) := storage(i).read(io.rdAddr.bits)
//    }
//  }
//  io.rdData := rdData
//
//  io.wrAddr.ready := Mux(io.rdAddr.valid, io.wrAddr.bits =/= io.rdAddr.bits, true.B)
//  when(io.wrAddr.valid && io.wrAddr.bits =/= io.rdAddr.bits) {
//    for(i <- 0 until c.nPorts) {
//      when(io.strobe(i)) {
//        storage(i).write(io.wrAddr.bits, io.wrData(i))
//      }
//    }
//  }
//}
