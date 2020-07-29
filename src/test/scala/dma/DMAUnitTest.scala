// See README.md for license details.

package dma

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

class TBDMATop(c:ChiselTopWrapper) extends PeekPokeTester(c) {
  println("Running ChiselTopWrapper!")
  step(300)
  //using gtkwave with the .vcd file for viewing ops
}

class ChiselTopWrapper(implicit p: Parameters) extends MultiIOModule {
  val top = Module(LazyModule(new CRTemp).module)
  val io = IO(top.io.cloneType)
  io <> top.io
}
