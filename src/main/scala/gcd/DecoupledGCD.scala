// See README.md for license details.

package gcd

import chisel3._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util.{Decoupled, Valid}
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes, HasCrossing}
import freechips.rocketchip.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.util._
import testchipip.TLHelper
import sifive.blocks.devices.chiplink._
import sifive.fpgashells.ip.microsemi.polarfireccc._
import sifive.fpgashells.devices.microsemi.polarfireevalkitpciex4._
import sifive.fpgashells.ip.microsemi.CLKINT
import sifive.fpgashells.ip.microsemi.polarfiredll._
import sifive.fpgashells.devices.microsemi.polarfireddr4._
import sifive.fpgashells.clocks._

class ClientTL(as: Seq[AddressSet], axi: Boolean = false, offset:UInt = 0.U)(implicit p: Parameters) extends LazyModule{
  val node = TLHelper.makeClientNode(TLMasterParameters.v1(
    name = "ClientTL",
    sourceId = IdRange(0, 4),
    requestFifo = true,
    visibility = as))
  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.out(0)
    val io = IO(Flipped(new TLBundle(edge.bundle)))
    tl.a.valid := io.a.valid
    tl.a.bits  := io.a.bits
    io.a.ready := tl.a.ready

    tl.d.ready := io.d.ready
    io.d.bits  := tl.d.bits
    io.d.valid := tl.d.valid
  }
}

class ManagerTL(as: Seq[AddressSet]) (implicit p: Parameters) extends LazyModule {
  val device = new SimpleDevice("ManagerTL", Seq())
  val beatBytes = 64
  val node = TLHelper.makeManagerNode(beatBytes, TLSlaveParameters.v1(
    address = as,
    resources = device.reg,
    regionType = RegionType.UNCACHED,
    executable = true,
    supportsGet = TransferSizes(1, 64),//beatBytes),
    supportsPutFull = TransferSizes(1, 64),//beatBytes),
    supportsPutPartial = TransferSizes(1, 64),//beatBytes),
    supportsArithmetic = TransferSizes(1, 64),
    supportsLogical = TransferSizes(1, 64),
    supportsHint = TransferSizes(1, 64)))
  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.in(0)
    val io = IO(new TLBundle(edge.bundle))
    io.a.valid := tl.a.valid
    io.a.bits  := tl.a.bits
    tl.a.ready := io.a.ready

    io.d.ready := tl.d.ready
    tl.d.bits  := io.d.bits
    tl.d.valid := io.d.valid
  }
}

class AXIClient(implicit p: Parameters) extends LazyModule with HasCrossing{
  val crossing = SynchronousCrossing()
  val node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name    = "axiMaster",
      id      = IdRange(0, 4),
      aligned = false)))))
  lazy val module = new LazyModuleImp(this) {
    val (m, _) = node.out(0)
    val io = IO(Flipped(AXI4Bundle(node.out(0)._2.bundle)))
    m.aw.bits   := io.aw.bits
    m.aw.valid  := io.aw.valid                          
    io.aw.ready := m.aw.ready

    m.w.bits    := io.w.bits
    m.w.valid   := io.w.valid    
    io.w.ready  := m.w.ready

    io.b.bits   := m.b.bits
    io.b.valid  := m.b.valid
    m.b.ready   := io.b.ready

    m.ar.valid  := io.ar.valid     
    m.ar.bits   := io.ar.bits
    io.ar.ready := m.ar.ready

    io.r.bits   := m.r.bits
    io.r.valid  := m.r.valid
    m.r.ready   := io.r.ready
  }
}

class AXIManager(implicit p: Parameters) extends LazyModule with HasCrossing {
  val crossing = SynchronousCrossing()
  val device = new SimpleDevice("manAXImanual", Seq())
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address       = List(AddressSet(0x400, 0xff)),
      supportsWrite = TransferSizes(1, 64),
      supportsRead  = TransferSizes(1, 64))),
    beatBytes = 8)))
  lazy val module = new LazyModuleImp(this) {
    val (s, _) = node.in(0)
    val io = IO(AXI4Bundle(node.in(0)._2.bundle))
    //module implementation
    io.aw.bits  := s.aw.bits
    io.aw.valid := s.aw.valid                          
    s.aw.ready  := io.aw.ready

    io.w.bits   := s.w.bits
    io.w.valid  := s.w.valid    
    s.w.ready   := io.w.ready

    s.b.bits    := io.b.bits
    s.b.valid   := io.b.valid
    io.b.ready  := s.b.ready

    io.ar.valid := s.ar.valid     
    io.ar.bits  := s.ar.bits
    s.ar.ready  := io.ar.ready

    s.r.bits    := io.r.bits
    s.r.valid   := io.r.valid
    io.r.ready  := s.r.ready
  }
}

class LazyAXITopZynq(implicit p: Parameters) extends LazyModule {
  //val manAXI = LazyModule(new AXIRegRouter(AddressSet(0x300, 0xff)))
  val manAXI= LazyModule(new AXIManager)
  val clerr = LazyModule(new TLError(DevNullParams(
        Seq(AddressSet(0x900, 0xff)), 8, 128, 
        region = RegionType.TRACKED), 
      buffer = false, 
      beatBytes = 4))
  val sbar = LazyModule(new TLXbar())
  val cl = LazyModule(new ChipLink(ChipLinkParams(
      TLUH = Seq(AddressSet(0x200, 0xff), AddressSet(0x300,0xff)),
      TLC  = Seq(AddressSet(0x500, 0xff)), syncTX = true)))
  val clsink = cl.ioNode.makeSink
  val axierr = LazyModule(new TLError(DevNullParams(
      Seq(AddressSet(0x800, 0xff)), 8, 128, 
      region = RegionType.UNCACHED), 
    beatBytes = 4))
  val mbar = LazyModule(new TLXbar)
  val cliAXI = LazyModule(new AXIClient)
  val convCliAXI:TLOutwardNode = (TLWidthWidget(4) 
    := AXI4ToTL() 
    := AXI4UserYanker(capMaxFlight=Some(8)) 
    := AXI4Fragmenter() 
    := AXI4IdIndexer(idBits=2) 
    :=  AXI4Buffer() 
    := cliAXI.node)

  manAXI.node  := (AXI4Buffer() 
    := AXI4UserYanker()  
    := AXI4Deinterleaver(64) 
    := AXI4IdIndexer(idBits=2) 
    := TLToAXI4() 
    := TLWidthWidget(4) 
    := sbar.node)
  clerr.node   :=  sbar.node
  sbar.node    := (TLBuffer() 
    := TLAtomicAutomata() 
    := TLFIFOFixer() 
    := TLHintHandler() 
    := TLBuffer() 
    := TLWidthWidget(4) 
    := cl.node)
  cl.node      := mbar.node
  axierr.node  := mbar.node
  mbar.node    := (TLFIFOFixer() 
    := cliAXI.crossTLOut(convCliAXI))

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val io_axi_s = manAXI.module.io.cloneType
      val io_axi_m = Flipped(cliAXI.module.io.cloneType)
      val chiplink = new WideDataLayerPort(ChipLinkParams(
          TLUH = Seq(AddressSet(0x300,0xff)),
          TLC  = Seq()))
    })

    io.chiplink      <> clsink.bundle

    //io_axi_m
    cliAXI.module.io.aw.bits  := io.io_axi_m.aw.bits
    cliAXI.module.io.aw.valid       := io.io_axi_m.aw.valid                          
    io.io_axi_m.aw.ready            := cliAXI.module.io.aw.ready
    cliAXI.module.io.w.bits   := io.io_axi_m.w.bits
    cliAXI.module.io.w.valid        := io.io_axi_m.w.valid    
    io.io_axi_m.w.ready             := cliAXI.module.io.w.ready
    io.io_axi_m.b.bits        := cliAXI.module.io.b.bits
    io.io_axi_m.b.valid             := cliAXI.module.io.b.valid
    cliAXI.module.io.b.ready        := io.io_axi_m.b.ready
    cliAXI.module.io.ar.bits  := io.io_axi_m.ar.bits
    cliAXI.module.io.ar.valid       := io.io_axi_m.ar.valid     
    io.io_axi_m.ar.ready            := cliAXI.module.io.ar.ready
    io.io_axi_m.r.bits        := cliAXI.module.io.r.bits
    io.io_axi_m.r.valid             := cliAXI.module.io.r.valid
    cliAXI.module.io.r.ready        := io.io_axi_m.r.ready

    //manAXI
    io.io_axi_s.aw.bits       := manAXI.module.io.aw.bits
    io.io_axi_s.aw.valid            := manAXI.module.io.aw.valid                          
    manAXI.module.io.aw.ready       := io.io_axi_s.aw.ready
    io.io_axi_s.w.bits        := manAXI.module.io.w.bits
    io.io_axi_s.w.valid             := manAXI.module.io.w.valid    
    manAXI.module.io.w.ready        := io.io_axi_s.w.ready
    manAXI.module.io.b.bits   := io.io_axi_s.b.bits
    manAXI.module.io.b.valid        := io.io_axi_s.b.valid
    io.io_axi_s.b.ready             := manAXI.module.io.b.ready
    io.io_axi_s.ar.bits       := manAXI.module.io.ar.bits
    io.io_axi_s.ar.valid            := manAXI.module.io.ar.valid     
    manAXI.module.io.ar.ready       := io.io_axi_s.ar.ready
    manAXI.module.io.r.bits   := io.io_axi_s.r.bits
    manAXI.module.io.r.valid        := io.io_axi_s.r.valid
    io.io_axi_s.r.ready             := manAXI.module.io.r.ready
  }
}

class LazyAXITopREDEFINE(implicit p: Parameters) extends LazyModule {
  val manTL1 = LazyModule(new ManagerTL(Seq(AddressSet(0x200, 0xff))))
  val clerr = LazyModule(new TLError(DevNullParams(
      Seq(AddressSet(0x900, 0xff)), 8, 128, 
      region = RegionType.TRACKED), 
    buffer = false, 
    beatBytes = 4))
  val sbar = LazyModule(new TLXbar())
  val cl = LazyModule(new ChipLink(ChipLinkParams(
      TLUH = Seq(AddressSet(0x200, 0xff), AddressSet(0x300,0xff)),
      TLC  = Seq(AddressSet(0x500, 0xff)), syncTX = true)))
  val clsink = cl.ioNode.makeSink
  val mbar = LazyModule(new TLXbar)
  val cliTL0 = LazyModule(new ClientTL(
    Seq(AddressSet(0x100, 0xff), AddressSet(0x200,0xff), AddressSet(0x300, 0xff))))

  manTL1.node  := (TLWidthWidget(4)  
    :=  sbar.node)
  clerr.node   :=  sbar.node
  sbar.node    := (TLBuffer() 
    := TLAtomicAutomata() 
    := TLFIFOFixer() 
    := TLHintHandler() 
    := TLBuffer() 
    := TLWidthWidget(4) 
    := cl.node)
  cl.node      := mbar.node
  mbar.node    := cliTL0.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val chiplink = new WideDataLayerPort(ChipLinkParams(
          TLUH = Seq(AddressSet(0x300,0xff)),
          TLC = Seq()))
      val tl_m  = Flipped(cliTL0.module.io.cloneType)
      val tl_s  = manTL1.module.io.cloneType
    })
    io.chiplink <> clsink.bundle

    //io_tl_m
    cliTL0.module.io.a.valid := io.tl_m.a.valid
    cliTL0.module.io.a.bits  := io.tl_m.a.bits
    io.tl_m.a.ready := cliTL0.module.io.a.ready
    cliTL0.module.io.d.ready := io.tl_m.d.ready
    io.tl_m.d.bits  := cliTL0.module.io.d.bits
    io.tl_m.d.valid := cliTL0.module.io.d.valid

    //io_tl_s
    io.tl_s.a.valid := manTL1.module.io.a.valid
    io.tl_s.a.bits  := manTL1.module.io.a.bits
    manTL1.module.io.a.ready :=io.tl_s.a.ready
    io.tl_s.d.ready := manTL1.module.io.d.ready
    manTL1.module.io.d.bits  :=io.tl_s.d.bits
    manTL1.module.io.d.valid :=io.tl_s.d.valid

  }
}

class ChiselTopWrapper(p: Parameters) extends MultiIOModule {
  implicit val _p = p
/*For REDEFINE chip*/
  val r = Module(LazyModule(new LazyAXITopREDEFINE).module)
  val io = IO(r.io.cloneType)
  io.chiplink <> r.io.chiplink

  //tl_m
  r.io.tl_m.a.valid := io.tl_m.a.valid
  r.io.tl_m.a.bits  := io.tl_m.a.bits
  r.io.tl_m.d.ready := io.tl_m.d.ready
  io.tl_m.a.ready   := r.io.tl_m.a.ready
  io.tl_m.d.bits    := r.io.tl_m.d.bits
  io.tl_m.d.valid   := r.io.tl_m.d.valid

  //tl_s
  io.tl_s.a.valid   := r.io.tl_s.a.valid
  io.tl_s.a.bits    := r.io.tl_s.a.bits
  io.tl_s.d.ready   := r.io.tl_s.d.ready
  r.io.tl_s.a.ready :=io.tl_s.a.ready
  r.io.tl_s.d.bits  :=io.tl_s.d.bits
  r.io.tl_s.d.valid :=io.tl_s.d.valid

/*For Zynq board*/
//  val z = Module(LazyModule(new LazyAXITopZynq).module)
//  val io = IO(z.io.cloneType)
//
//  io.chiplink <> z.io.chiplink
//
//  //io_axi_m
//  z.io.io_axi_m.aw.bits  := io.io_axi_m.aw.bits
//  z.io.io_axi_m.aw.valid := io.io_axi_m.aw.valid                          
//  io.io_axi_m.aw.ready   := z.io.io_axi_m.aw.ready
//  z.io.io_axi_m.w.bits   := io.io_axi_m.w.bits
//  z.io.io_axi_m.w.valid  := io.io_axi_m.w.valid    
//  io.io_axi_m.w.ready    := z.io.io_axi_m.w.ready
//  io.io_axi_m.b.bits     := z.io.io_axi_m.b.bits
//  io.io_axi_m.b.valid    := z.io.io_axi_m.b.valid
//  z.io.io_axi_m.b.ready  := io.io_axi_m.b.ready
//  z.io.io_axi_m.ar.bits  := io.io_axi_m.ar.bits
//  z.io.io_axi_m.ar.valid := io.io_axi_m.ar.valid     
//  io.io_axi_m.ar.ready   := z.io.io_axi_m.ar.ready
//  io.io_axi_m.r.bits     := z.io.io_axi_m.r.bits
//  io.io_axi_m.r.valid    := z.io.io_axi_m.r.valid
//  z.io.io_axi_m.r.ready  := io.io_axi_m.r.ready
//
//  //io_axi_s
//  io.io_axi_s.aw.bits    :=z.io.io_axi_s.aw.bits
//  io.io_axi_s.aw.valid   :=z.io.io_axi_s.aw.valid                          
//  z.io.io_axi_s.aw.ready :=io.io_axi_s.aw.ready
//  io.io_axi_s.w.bits     :=z.io.io_axi_s.w.bits
//  io.io_axi_s.w.valid    :=z.io.io_axi_s.w.valid    
//  z.io.io_axi_s.w.ready  :=io.io_axi_s.w.ready
//  z.io.io_axi_s.b.bits   :=io.io_axi_s.b.bits
//  z.io.io_axi_s.b.valid  :=io.io_axi_s.b.valid
//  io.io_axi_s.b.ready    :=z.io.io_axi_s.b.ready
//  io.io_axi_s.ar.bits    :=z.io.io_axi_s.ar.bits
//  io.io_axi_s.ar.valid   :=z.io.io_axi_s.ar.valid     
//  z.io.io_axi_s.ar.ready :=io.io_axi_s.ar.ready
//  z.io.io_axi_s.r.bits   :=io.io_axi_s.r.bits
//  z.io.io_axi_s.r.valid  :=io.io_axi_s.r.valid
//  io.io_axi_s.r.ready    :=z.io.io_axi_s.r.ready

}
