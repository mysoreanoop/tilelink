package DMA

import chisel3._
import chisel3.util.Queue
import testchipip.TLHelper

class PortParam(implicit p: Parameters) (flag:Boolean) extends Bundle {
//TODO use parameterized bundle instead
  val xStep, yStep = UInt(p(DMAParams).addrCtrlWidth.W)
  val xCnt, yCnt   = UInt(p(DMAParams).addrCtrlWidth.W)
  val addr         = UInt(p(DMAParams).addrWidth.W)
  val nodeId       = if(flag) Some(UInt(p(DMAParams).idWidth.W)) else None
}

class CSRBundle(implicit p: Parameters) extends Bundle {
  val src    = new PortParam(0)
  val dest   = new PortParam(0)
  val txnId  = UInt(log2Up(p(DMAParams).maxDMAReqs).W)
}

class DMA(CRId: Int)(implicit p: Parameters) extends LazyModule {
  val beatBytes = p(DMAParams).beatBytes
  val noc = TLHelper.makeManagerNode(beatBytes, TLSlaveParameters.v1(
    address = AddressSet(0x0, 0xffff),
    //resources = device.reg, //TODO
    regionType = RegionType.UNCACHED,
    executable = true, //TODO
    supportsGet = TransferSizes(1, beatBytes),
    supportsPutFull = TransferSizes(1, beatBytes),
    supportsPutPartial = TransferSizes(1, beatBytes),
    supportsArithmetic = TransferSizes(1, beatBytes),
    supportsLogical = TransferSizes(1, beatBytes),
    fifoId = Some(0))) //TODO
  //local DSM
  val dsm = TLHelper.makeManagerNode(beatBytes, TLSlaveParameters.v1(
    address = AddressSet(0x0, p(DSMParams).DSMSize-1),
    //resources = device.reg, //TODO
    regionType = RegionType.UNCACHED,
    executable = true, //TODO
    supportsGet = TransferSizes(1, beatBytes),
    supportsPutFull = TransferSizes(1, beatBytes),
    supportsPutPartial = TransferSizes(1, beatBytes),
    supportsArithmetic = TransferSizes(1, beatBytes),
    supportsLogical = TransferSizes(1, beatBytes),
    fifoId = Some(0))) //TODO
  val module = DMAModule(this)
}

class DMAModule(outer: DMA) extends LazyModuleImpl(outer) {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CSRBundle(p)))
    val status = Valid(UInt(log2Up(p(DMAParams).maxDMAReqs).W))
  }
  val c = p(DMAParams)

  //TODO for transactions between 2 agents only accessible via noc, we could invoke 
  //rrm's dma; otherwise, the read and write bursts cannot overlap!
  val noc = outer.noc.in(0)
  val dma = outer.dsm.in(0)
  
  val ctrl = Queue(p(DMAParams).nOutstanding, io.in)
  //Provide xCount in terms of words!
  //
  val rdAddr = Module(new AddressGenerator).io
  val wrAddr = Module(new AddressGenerator).io
  when(ctrl.src.id === 0.U) {
    rdAddr.cmd := ctrl.src
    wrAddr.cmd := ctrl.dest
  } .otherwise {
    rdAddr.cmd := ctrl.dest
    wrAddr.cmd := ctrl.src
  }

  val queue = Module(new Queue(UInt(c.dataWidth.W), c.fifoDepth, true, true)).io
  queue.enq <> Mux(ctrl.bits.src.id, noc.d, dsm.d)
  val get = edge.Get(fromSource = ctrl.src.id, toAddress = rdAddr.out.addr, lgSize = rdAddr.out.len)._2
  val put = edge.Put(fromSource = ctrl.dest.id, toAddress = wrAddr.out.addr, lgSize = wrAddr.out.len, data = queue.deq.bits, mask = wrAddr.out.strobe)
  
  /* Holds the latest put transaction status;
   * In case something fails, we could look into this.*/
  val debugResp := RegInit(Mux(ctrl.bits.src.id, dsm.d.bits, noc.d.bits))

  /* Future work:
   * Between the queue and writer, and just before the reader, we could introduce
   * the bus holding logic.*/

  when(ctrl.valid) {
    when(ctrl.bits.src.id === 0.U) {
      noc.a.bits := put
      noc.a.valid := queue.deq.valid
      queue.deq.ready := noc.a.ready
      dsm.a.bits := get
    } .otherwise {
      noc.a.bits := get
      dsm.a.bits := put
      dsm.a.valid := queue.deq.valid
      queue.deq.ready := dsm.a.ready
    }
  }
}

/* Produces address to read from or write to.
 * To be used in the right context depending on which agent is the source.
 */
class AddressGenerator(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val out  = Output(new Bundle{
      val addr = UInt(p(DMAParams).addrWidth.W)
      val strobe = UInt(p(DMAParams).strobeWidth.W)
      val len = UInt(log2Up(DMAParams).maxBurst.W)
    })
    val cmd   = Flipped(Valid(new PortParam(false)))
    val inUse = Output(Bool())
    val req   = Input(Bool())
  })

  val cmd = RegInit({val n = Wire(PortParams(false));
    n.foreach{i := 0.U}; n})
  val mask = RegInit(Fill(p(DMAParams).strobeWidth, 1.U))
  val len  = RegInit(p(DMAParams).maxBurst.U(log2Up(DMAParams).maxBurst.W)

  def calculate(n:PortParams(false)) = {
    val out = Wire(new PortParam(false))
    out := n
    when(n.xCnt > 0.U) {
      //out.xCnt := n.xCnt - 1.U 
      /*The below incorporates busWidth-burst advantages.*/
      when(n.xCnt >= p(DMAParams).busWidth * p(DMAParams).maxBurst / wordWidth) {
        out.xCnt := n.xCnt - (p(DMAParams).busWidth * p(DMAParams).maxBurst / wordWidth).U
        mask := Fill(p(DMAParams).strobeWidth, 1.U)
        len := p(DMAParams).maxBurst.U
        out.addr := n.addr + (p(DMAParams).busWidth * p(DMAParams).maxBurst * p(DMAParams).dataWidth/8).U //considering xStep = 1
      } .otherwise {
        //Cut it up into as many bursts and one final masked transaction
        //Determining one big <maxBurst burst requires division
        len  := 1.U
        when(n.xCnt >= (p(DMAParams).busWidth/8).U) {
          mask := Fill(p(DMAParams).strobeWidth, 1.U)
          out.xCnt := n.xCnt - (p(DMAParams).busWidth/8).U
          out.addr := n.addr + (p(DMAParams).busWidth * p(DMAParams).dataWidth/8).U
          //if xCnt becomes zero, then it goes to the xCnt===0 loop, which is fine
        } .otherwise {//last txn
          mask := ~(strobeWidth - n.xCnt)
          out.xCnt := 0.U
          out.addr := n.addr + (n.xCnt * p(DMAParams).dataWidth/8).U
        }
      }
    } .otherwise {//.elsewhen(n.xCnt === 0.U) {
      when(n.yCnt > 0.U) {
        out.xCnt := xCntOrig
        out.addr := n.addr + n.yStep * (p(DMAParams).dataWidth/8).U
        //yStep makes sense, xStep doesn't!
        out.yCnt := n.yCnt - 1.U
      }//Done!
    }
    out
  }

  io.inUse    := (bun.yCnt =/= 0.U) || (bun.xCnt =/= 0.U)
  io.out.addr := Mux(io.cmd.valid, io.cmd.bits.addr, cmd.addr)
  io.out.mask := mask
  when(io.req && ~io.cmd.valid) {
    cmd := calculate(cmd)
  } .elsewhen(io.cmd.valid) {
    xCntOrig := io.cmd.bits.xCnt
    cmd := Mux(io.req, calculate(io.cmd.bits), io.cmd.bits)
    assert(io.cmd.bits.xStep === 1, "Address increment after every burst != 1; This leads to a very inefficient mode of data transfer!")
  }
}

