package dma

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util._
import testchipip.TLHelper

class CRTemp(implicit p: Parameters) extends LazyModule {
  //Can be used when we need to verify if we're reading appropriately

  //val rom = LazyModule(new TLROM(
  //  base = 0,
  //  size = 0x10000,
  //  contentsDelayed = Seq.tabulate(0x10000) {i=>i.toByte},
  //  beatBytes = 32))
  //val ram0 = LazyModule(new ManagerTL)
  val rom = LazyModule(new TLRAM(address=AddressSet(0, 0xfffff), 
    beatBytes = 32))
  val ram = LazyModule(new TLRAM(address=AddressSet(0, 0xffffff), 
    beatBytes = 32))

  val dma  = LazyModule(new DMA)
  rom.node := TLFragmenter(32, p(DMAKey).maxBurst * 32) := dma.dsm
  ram.node := TLFragmenter(32, p(DMAKey).maxBurst * 32) := dma.noc

  lazy val module = new LazyModuleImp(this) {
    val io = IO(dma.module.io.cloneType)
    io <> dma.module.io
  }
}
