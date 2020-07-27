package DMA

import chisel3._
import chisel3.util.Queue
import testchipip.TLHelper

class PortParam(addrWidth: Int, idWidth:Int, addrCtrlWidth: Int) extends Bundle {
//TODO use parameterized bundle instead
  val xStep, xCnt, yStep, yCnt  = UInt(addrCtrlWidth.W)
  val addr   = UInt(addrWidth.W)
  val nodeId = UInt(idWidth.W)
}

class CSRBundle(addrWidth: Int, idWidth:Int, addrCtrlWidth: Int, maxDMAAgents: Int) extends Bundle {
  val src    = new PortParam(addrWidth, idWidth, addrCtrlWidth)
  val dest   = new PortParam(addrWidth, idWidth, addrCtrlWidth)
  val txnId  = UInt(log2Up(maxDMAReqs).W)
}

class DMA(CRId: Int)(implicit p: Parameters) extends LazyModule {
  val beatBytes = p(DMAParams).beatBytes
  val node0 = TLHelper.makeManagerNode(beatBytes, TLSlaveParameters.v1(
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
  val node1 = TLHelper.makeManagerNode(beatBytes, TLSlaveParameters.v1(
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
    val status = Valid(UInt(log2Up(maxDMAReqs).W))
  }

  //TODO for transactions between 2 agents only accessible via noc, we could invoke 
  //rrm's dma; otherwise, the read and write bursts cannot happen concurrently!
  val dsm = outer.node0.in(0)
  val noc = outer.node1.in(0)
  
  val ctrl = Queue(p(DMAParams).nOutstanding, io.in)

  val c = p(DMAParams)
  val queue = Module(new Queue(UInt(c.dsmDataWidth.W), c.fifoDepth, true, true)).io

  val rdAddr = Module(new AddressGenerator).io
  queue.enq <> rdAddr.q
  val wrAddr = Module(new AddressGenerator).io
  wrAddr.q  <> queue.deq
  when(ctrl.src.id === 0.U) {
    rdAddr.param := ctrl.src
    rdAddr.node  := dsm
  } .otherwise {
    rdAddr.param := ctrl.dest
    rdAddr.node  := noc
  }

  readerFrontend.io.xfer <> ctl.io.xferRead
  writerFrontend.io.xfer <> ctl.io.xferWrite
  node0 <> readerFrontend.io.bus
  node1 <> writerFrontend.io.bus
}



