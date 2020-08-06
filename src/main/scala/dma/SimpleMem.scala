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
case class DSMParams(
    temp: Boolean,
    dsmSize: BigInt = 0x10000,
    nPorts: Int = 8,
    dataWidth: Int = 32,
    addrWidth: Int = 32) {
  require(dsmSize%nPorts==0, "DSM size has to be in multiples of port size")
}

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
