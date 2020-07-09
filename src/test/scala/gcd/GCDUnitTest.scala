// See README.md for license details.

package gcd

import java.io.File

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

class TBLazyTop(c:ChiselTopWrapper) extends PeekPokeTester(c) {
  println("Running ChiselTopWrapper!")
  step(300)
  //using gtkwave with the .vcd file for viewing ops
}

class ChiselTopWrapper(p: Parameters) extends MultiIOModule {
  implicit val _p = p
  val top0 = Module(LazyModule(new LazyAXITop).module)
  val top1 = Module(LazyModule(new LazyAXITop).module)
  val io = IO(new Bundle{
    val t0 = top0.io.cloneType
    val t1 = top1.io.cloneType
  })
  io.t0 <> top0.io
  io.t1 <> top1.io
  top0.io.chiplink.b2c :=  top1.io.chiplink.c2b
  top1.io.chiplink.b2c :=  top0.io.chiplink.c2b
}
