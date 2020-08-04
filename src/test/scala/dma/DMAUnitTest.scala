// See README.md for license details.

package dma

import chisel3.iotesters
import chisel3.iotesters._
import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

class TBDMATop(c:ChiselTopWrapper) extends PeekPokeTester(c) {
//  var k = 0
//  println("Running ChiselTopWrapper!")
//  poke(c.io.cmd.valid, 1)
//  poke(c.io.cmd.bits.addr, 0)
//  poke(c.io.cmd.bits.nodeId, 0)
//  poke(c.io.cmd.bits.xStep, 1)
//  poke(c.io.cmd.bits.yStep, 0x100)
//  poke(c.io.cmd.bits.xCnt, 0x1B3)
//  poke(c.io.cmd.bits.yCnt, 0x4)
//  while(k < 200) {
//    poke(c.io.out.ready, 1)
//    step(1)
//    poke(c.io.out.ready, 0)
//    k = k+1
//    step(1)
//  }
//  step(100)
  
  println("Running ChiselTopWrapper!")
  step(5)
  poke(c.io.in.valid, 1)
  poke(c.io.in.bits.src.addr, 0)
  poke(c.io.in.bits.src.nodeId, 0)
  poke(c.io.in.bits.src.xStep, 1)
  poke(c.io.in.bits.src.yStep, 0x100)
  poke(c.io.in.bits.src.xCnt, 0x100)
  poke(c.io.in.bits.src.yCnt, 0x4)
  poke(c.io.in.bits.dest.addr, 0)
  poke(c.io.in.bits.dest.nodeId, 23)
  poke(c.io.in.bits.dest.xStep, 1)
  poke(c.io.in.bits.dest.yStep, 0x100)
  poke(c.io.in.bits.dest.xCnt, 0x100)
  poke(c.io.in.bits.dest.yCnt, 0x4)
  step(10)
  step(500)
}

class ChiselTopWrapper(implicit p: Parameters) extends MultiIOModule {
  val top = Module(LazyModule(new CRTemp).module)
  val io = IO(top.io.cloneType)
  io <> top.io
 // val top = Module(new AddressGenerator)
 // val io = IO(top.io.cloneType)
 // io <> top.io
}
