// See README.md for license details.

package dma

import chisel3.iotesters
import chisel3.iotesters._
import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

class TBDMATop(c:ChiselTopWrapper) extends PeekPokeTester(c) {
  println("Running ChiselTopWrapper!")
  poke(c.io.cmd.valid, 1)
  poke(c.io.cmd.bits.addr, 0)
  poke(c.io.cmd.bits.nodeId, 0)
  poke(c.io.cmd.bits.xStep, 1)
  poke(c.io.cmd.bits.yStep, 1)
  poke(c.io.cmd.bits.xCnt, 0x80)
  poke(c.io.cmd.bits.yCnt, 2)
  step(305)
  //using gtkwave with the .vcd file for viewing ops
}

class ChiselTopWrapper(implicit p: Parameters) extends MultiIOModule {
  //val top = Module(LazyModule(new CRTemp).module)
  //val io = IO(top.io.cloneType)
  //io <> top.io
  //
  val top = Module(new AddressGenerator)
  val io = IO(top.io.cloneType)
  io <> top.io
}
