// See README.md for license details.

package dma

import chisel3.iotesters
import chisel3.iotesters._
import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

class TBDMATop(c:ChiselTopWrapper) extends PeekPokeTester(c) {
  var k = 0
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
  poke(c.io.cmd.src.addr, 0x100)
  poke(c.io.cmd.src.nodeId, 0)
  poke(c.io.cmd.src.xStep, 1)
  poke(c.io.cmd.src.yStep, 0x100)
  poke(c.io.cmd.src.xCnt, 0x100)
  poke(c.io.cmd.src.yCnt, 0x4)
  poke(c.io.cmd.dest.addr, 0x400)
  poke(c.io.cmd.dest.nodeId, 23)
  poke(c.io.cmd.dest.xStep, 1)
  poke(c.io.cmd.dest.yStep, 0x100)
  poke(c.io.cmd.dest.xCnt, 0x100)
  poke(c.io.cmd.dest.yCnt, 0x4)
  poke(c.io.cmd.start, 1)
  poke(c.io.cmd.mode, 1) //DSM to NoC
  while((peek(c.io.done) == 0) && k < 600) {
    step(1)
    k = k+1
  }
  poke(c.io.cmd.start, 0)
  step(10)
}

class ChiselTopWrapper(implicit p: Parameters) extends MultiIOModule {
  val top = Module(LazyModule(new CRTemp).module)
  val io = IO(top.io.cloneType)
  io <> top.io
 // val top = Module(new AddressGenerator)
 // val io = IO(top.io.cloneType)
 // io <> top.io
}
