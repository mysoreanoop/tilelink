// See README.md for license details.

package dma

import chisel3.iotesters
import chisel3.iotesters._
import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

class TBDMATop(c:ChiselTopWrapper) extends PeekPokeTester(c) {
  println("Running ChiselTopWrapper!")
  poke(c.io.other.in.valid, 1)
  poke(c.io.other.in.bits.src.addr, 0x100)
  poke(c.io.other.in.bits.src.nodeId, 0)
  poke(c.io.other.in.bits.src.xStep, 1)
  poke(c.io.other.in.bits.src.yStep, 1)
  poke(c.io.other.in.bits.src.xCnt, 0x80)
  poke(c.io.other.in.bits.src.yCnt, 0x100)
  poke(c.io.other.in.bits.dest.addr, 0x400)
  poke(c.io.other.in.bits.dest.nodeId, 0)
  poke(c.io.other.in.bits.dest.xStep, 1)
  poke(c.io.other.in.bits.dest.yStep, 1)
  poke(c.io.other.in.bits.dest.xCnt, 0x80)
  poke(c.io.other.in.bits.dest.yCnt, 0x100)
  poke(c.io.other.in.bits.txnId, 0x4)
  step(305)
  //using gtkwave with the .vcd file for viewing ops
}

class ChiselTopWrapper(implicit p: Parameters) extends MultiIOModule {
  val top = Module(LazyModule(new CRTemp).module)
  val io = IO(top.io.cloneType)
  io <> top.io
}
