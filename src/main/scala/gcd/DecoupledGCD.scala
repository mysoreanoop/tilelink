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

/*A client device to write via ChipLink (to an outside device)*/
class ClientCL(as: Seq[AddressSet])(implicit p: Parameters) extends LazyModule with HasCrossing{
  val crossing = SynchronousCrossing()
  val node = TLHelper.makeClientNode(TLMasterParameters.v1(
    name = "ClientCL",
    sourceId = IdRange(5, 9),
    requestFifo = true,
    visibility = as))
  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.out(0)
    val io = IO(new Bundle{
      val tla = Valid(UInt(8.W))
      val tld = Valid(UInt(8.W))
      val state  = Output(UInt(5.W))
      val counter = Output(UInt(8.W))
    })
    //State machine to inject data onto tl_a and tl_d appropriately
    val state = RegInit(0.U(5.W))
    //Just to make sure nothing is a coincidence
    val counter = RegInit(0.U(8.W))

    io.tla.bits := tl.a.bits.data
    io.tld.bits := tl.d.bits.data
    io.tla.valid := tl.a.valid
    io.tld.valid := tl.d.valid
    io.state := state
    io.counter := counter

    val writtenValue = RegInit(0.U(8.W))

    //edge object methods to make things easier?
    val put0 = edge.Put(fromSource = 5.U, 
      toAddress = "h300".U, 
      lgSize = 1.U, 
      data = "h36".U)._2
    val get0 = edge.Get(fromSource = 8.U, 
      toAddress = "h300".U, 
      lgSize = 1.U)._2

    //init wires
    tl.a.valid := false.B
    tl.a.bits  := DontCare
    tl.d.ready := false.B
    counter := counter+1.U

    //for use during state switching
    val completed = RegInit(0.U(4.W))

    when(state === 0.U && counter > 35.U) {
      tl.a.valid := true.B
      tl.a.bits := put0 //putting first variable
      tl.d.ready := true.B
      when(tl.a.fire()) {
        when(tl.d.fire()) {
          state := 2.U
        } .otherwise {
          state := 1.U
        }
        completed := 0.U
        writtenValue := tl.a.bits.data
      }
    } .elsewhen(state === 1.U) {//waiting state
      when(tl.d.fire()) {
        counter := 0.U
        writtenValue := tl.d.bits.data //garbage
        when(completed === 0.U) {
          state := 2.U
        }
      }
    } .elsewhen(state === 2.U) {
      tl.a.valid := true.B
      tl.a.bits := get0//getting a variable
      tl.d.ready := true.B
      when(tl.a.fire()) { 
        when(tl.d.fire()) {
          state := 3.U
        } .otherwise {
          state := 1.U
        }
        completed := 2.U
        writtenValue := tl.a.bits.data
      }
    }
  }
}

/*A client to write to various managers (local and foreign); may need to modify the addresses appropriately */
class ClientTL(as: Seq[AddressSet], axi: Boolean = false, offset:UInt = 0.U)(implicit p: Parameters) extends LazyModule{
  val node = TLHelper.makeClientNode(TLMasterParameters.v1(
    name = "ClientTL",
    sourceId = IdRange(0,4),
    requestFifo = true,
    visibility = as))
  lazy val module = new LazyModuleImp(this) {
    val (tl, edge) = node.out(0)
    val io = IO(new Bundle{
      val tla = Valid(UInt(8.W))
      val tld = Valid(UInt(8.W))
      val state  = Output(UInt(5.W))
      val counter = Output(UInt(8.W))
    })
    val state = RegInit(0.U(5.W))
    val counter = RegInit(0.U(8.W))
    io.tla.bits := tl.a.bits.data
    io.tld.bits := tl.d.bits.data
    io.tla.valid := tl.a.valid
    io.tld.valid := tl.d.valid
    io.state := state
    io.counter := counter
    val writtenValue = RegInit(0.U(8.W))

    /* changed for REDEFINE: local Man addr to foreign Man*/
    val putX = edge.Put(fromSource = 0.U, 
      toAddress = if(axi) offset else "h300".U, 
      lgSize = 1.U, 
      data = "h36".U)._2
    val putY = edge.Put(fromSource = 1.U, 
      toAddress = if(axi) offset+8.U else "h308".U, 
      lgSize = 1.U, 
      data = "h42".U)._2
    val putTL = edge.Put(fromSource = 2.U,//reuse 
      toAddress = if(axi) offset else "h300".U, 
      lgSize = 1.U, 
      data = "h23".U)._2
    val putAXI = edge.Put(fromSource = 3.U,//reuse 
      toAddress = if(axi) offset else "h308".U, 
      lgSize = 1.U, 
      data = "h73".U)._2
    val getGCD = edge.Get(fromSource = 3.U, 
      toAddress = if(axi) offset+16.U else "h310".U, 
      lgSize = 1.U)._2
    tl.a.valid := false.B
    tl.a.bits  := DontCare
    tl.d.ready := false.B
    counter := counter+1.U
    val completed = RegInit(0.U(5.W))
    when(state === 0.U && counter > 5.U) { //write x
      tl.a.valid := true.B
      tl.a.bits := putX
      tl.d.ready := true.B
      when(tl.a.fire()) {
        when(tl.d.fire()) {
          state := 2.U
        } .otherwise {
          state := 1.U
        }
        completed := 0.U
        writtenValue := tl.a.bits.data
      }
    } .elsewhen(state === 1.U) {//waiting state
      tl.d.ready := true.B
      when(tl.d.fire()) {
        counter := 0.U
        writtenValue := tl.d.bits.data //garbage
        when(completed === 0.U) {
          state := 2.U
        } .elsewhen(completed === 2.U) {
          state := 3.U
        } .elsewhen(completed === 3.U) {
          state := 4.U
        } .elsewhen(completed === 4.U) {
          state := 5.U
        } .otherwise {
          state := 6.U
        }
      }
    } .elsewhen(state === 2.U) { //write y
      tl.a.valid := true.B
      tl.a.bits := putY
      tl.d.ready := true.B
      when(tl.a.fire()) {
        when(tl.d.fire()) {
          state := 3.U
        } .otherwise {
          state := 1.U
        }
        completed := 2.U
        writtenValue := tl.a.bits.data
      }
    } .elsewhen(state === 3.U) { //get computed gcd(x,y)
      when(counter > "h10".U) {
        tl.a.valid := true.B//to wait for gcd to complete
        //TODO explore TL interrupts
      }
      tl.a.bits := getGCD
      tl.d.ready := true.B
      when(tl.a.fire()) {
        when(tl.d.fire()) {
          state := 4.U
        } .otherwise {
          state := 1.U
        }
        completed := 3.U
        writtenValue := tl.a.bits.data
      }
    } .elsewhen(state === 4.U) { //write via chiplink to TL dev
      tl.a.valid := true.B
      tl.a.bits := putTL
      tl.d.ready := true.B
      when(tl.a.fire()) {
        when(tl.d.fire()) {
          state := 5.U
        } .otherwise {
          state := 1.U
        }
        completed := 4.U
        writtenValue := tl.a.bits.data
      }
    }.elsewhen(state === 5.U) { //write via chiplink to AXI dev
      tl.a.valid := true.B
      tl.a.bits := putAXI
      tl.d.ready := true.B
      when(tl.a.fire()) {
        when(tl.d.fire()) {
          state := 6.U
        } .otherwise {
          state := 1.U
        }
        completed := 5.U
        writtenValue := tl.a.bits.data
      }
    }
  }
}

trait ManagerRegTLModule extends HasRegMap {
  implicit val p: Parameters
  val io: ManagerRegTLBundle
  val x = RegInit(0.U(8.W))
  val y = RegInit(0.U(8.W))
  val gcd = RegInit(0.U(8.W))
  io.x := x
  io.y := y
  io.gcd := gcd
  //module implementation
  regmap(
      0x00 -> Seq(RegField.w(8, x)),
      0x08 -> Seq(RegField(8, y)),
      0x10 -> Seq(RegField(8,gcd)))
  val gcdm = Module(new DecoupledGcd(8))
  gcdm.input.valid := x =/= y
  gcdm.input.bits.value1 := y
  gcdm.input.bits.value2 := x
  gcdm.output.ready := false.B
  when(gcdm.output.valid) {
    gcd := gcdm.output.bits.gcd
    gcdm.output.ready := true.B
  }
}

trait ManagerRegTLBundle extends Bundle {
  val x = Output(UInt(8.W))
  val y = Output(UInt(8.W))
  val gcd = Output(UInt(8.W))
}

case class ManagerRegTLParams(address:BigInt = 0x100, beatBytes:Int = 64) 
class ManagerRegTL(c: ManagerRegTLParams) (implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "ManagerRegTL", Seq(), size = 256, concurrency = 1, beatBytes=c.beatBytes)(
      new TLRegBundle(c, _)    with ManagerRegTLBundle)(
      new TLRegModule(c, _, _) with ManagerRegTLModule)

class ManagerTL(as: Seq[AddressSet]) (implicit p: Parameters) extends LazyModule {
  /* ManagerTL (TLRegisterRouter) also declares a similar SimpleDevice internally which can be modified by passing appropriate parameters alongside the TLRegisterRouter parameters */
  val device = new SimpleDevice("ManagerTL", Seq())
  val beatBytes = 8
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
    //fifoId = Some(0)))
  lazy val module = new LazyModuleImp(this) {
    val io = IO(Output(UInt(8.W)))
    val reg = RegInit(0.U(8.W))
    io := reg
    val (tl, edge) = node.in(0)
    tl.d.valid := false.B
    tl.a.ready := true.B
    when(tl.a.fire()) {
      reg := tl.a.bits.data //based on address, source, etc
      tl.d.valid := true.B
      tl.d.bits := edge.AccessAck(tl.a.bits) //refer to Edge Object Methods in Chipyard PDF
    }
  }
}

class ClientAXI(implicit p: Parameters) extends LazyModule with HasCrossing{
  val crossing = SynchronousCrossing()
  val node = AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name    = "ClientAXI",
      id      = IdRange(0, 16),
      aligned = false)))))
  lazy val module = new LazyModuleImp(this) {
    val (m, _) = node.out(0)
    //module implementation
    val counter= RegInit(0.U(8.W))
    counter := counter + 1.U
    val state = RegInit(0.U(5.W))
    val completed = RegInit(0.U(5.W))
   
    m.aw.bits.cache := 0.U
    m.aw.bits.prot  := 0.U
    m.aw.bits.qos   := 0.U
    m.aw.bits.id    := 0.U
    when(completed === 1.U) {
      m.aw.bits.addr  := "h200".U //via chiplink to tl
    } .elsewhen(completed === 2.U) {
      m.aw.bits.addr  := "h208".U //via cl to axi
    } .otherwise {
      //changed for REDEFINE
      m.aw.bits.addr  := "h210".U //local tl
    }  
    m.aw.bits.len   := 0.U
    m.aw.bits.size  := 1.U
    m.aw.bits.burst := 0.U
    m.aw.bits.id    := 0.U
    m.aw.valid      := false.B
    when(completed === 1.U) {
      m.w.bits.data  := "hBF".U //via chiplink to tl
    } .elsewhen(completed === 2.U) {
      m.w.bits.data  := "h12".U //via cl to axi
    } .otherwise {
      m.w.bits.data  := "h44".U //local tl
    }  
    m.w.bits.strb   := "h3".U
    m.w.bits.last   := true.B
    m.w.valid       := false.B
    m.b.ready       := false.B
    when(state === 0.U && counter >50.U) {
      m.aw.valid := true.B
      m.w.valid  := true.B
      m.b.ready  := true.B
      when(m.aw.fire()) {
        when(m.w.fire()) {
          state := 2.U
        } .otherwise {
          state := 1.U
        }
      }
    } .elsewhen( state === 1.U) {
      m.w.valid  := true.B
      m.b.ready  := true.B
      when(m.w.fire()) {
        when(m.b.fire()) {
          state := 3.U
        } .otherwise {
          state := 2.U
        }
      }
    } .elsewhen( state === 2.U) {
      m.b.ready  := true.B
      when(m.b.fire()) {
        state := 0.U
        completed := completed + 1.U
      }
    }
  }
}

class ManagerAXI(implicit p: Parameters) extends LazyModule with HasCrossing {
  val crossing = SynchronousCrossing()
  val device = new SimpleDevice("ManagerAXI", Seq())
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address       = List(AddressSet(0x400, 0xff)),
      supportsWrite = TransferSizes(1, 64),
      supportsRead  = TransferSizes(1, 64))),
    beatBytes = 8)))
  lazy val module = new LazyModuleImp(this) {
    val (m, _) = node.in(0)
    //module implementation
  }
}


class ManagerRegAXI(as: AddressSet)(implicit p: Parameters) extends LazyModule with HasCrossing{//manager
  val crossing = SynchronousCrossing()
  val node = AXI4RegisterNode(
      address = as,
      beatBytes = 64,
      concurrency = 0)
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val a = Output(UInt(8.W))
      val b = Output(UInt(8.W))
      val c = Output(UInt(8.W))
    })
    val a = RegInit(0.U(8.W))
    val b = RegInit(0.U(8.W))
    val c = RegInit(0.U(8.W))
    node.regmap(
        0x00 -> Seq(RegField(8, a)),
        0x08 -> Seq(RegField(8, b)),
        0x10 -> Seq(RegField(8, c)))
    io.a := a
    io.b := b
    io.c := c
  }
}

/* An example connecting TL client to AXI manager*/
//class LazyAXITop(implicit p: Parameters) extends LazyModule {
//  val man = LazyModule(new ManagerRegAXI(AddressSet(0x700, 0xff)))
//  val cli = LazyModule(new ClientTL(Seq(AddressSet(0x100, 0xff), AddressSet(0x700, 0xff)), true))
//  man.node  := TLToAXI4() := TLWidthWidget(4) := cli.node
//  lazy val module = new LazyModuleImp(this) {
//    val io = IO(new Bundle {
//      val a = Output(UInt(8.W))
//      val b = Output(UInt(8.W))
//      val c = Output(UInt(8.W))
//    })
//    io <> man.module.io
//  }
//}
//
///* An example connecting TL client to AXI manager*/
//class LazyAXITop(implicit p: Parameters) extends LazyModule {
//  val man = LazyModule(new ManagerRegAXI(AddressSet(0x700, 0xff)))
//  val cli = LazyModule(new ClientTL(Seq(AddressSet(0x100, 0xff), AddressSet(0x700, 0xff)), true))
//  man.node  := TLToAXI4() := TLWidthWidget(4) := cli.node
//  lazy val module = new LazyModuleImp(this) {
//    val io = IO(new Bundle {
//      val a = Output(UInt(8.W))
//      val b = Output(UInt(8.W))
//      val c = Output(UInt(8.W))
//    })
//    io <> man.module.io
//  }
//}
//
///* Combination of above two*/
//class LazyAXITop(implicit p: Parameters) extends LazyModule {
//  val manAXI = LazyModule(new ManagerRegAXI(AddressSet(0x700, 0xff)))
//  val cliTL = LazyModule(new ClientTL(Seq(AddressSet(0x100, 0xff), AddressSet(0x700,0xff)), true, "h700".U))
//  val manTL = LazyModule(new ManagerTL(Seq(AddressSet(0x300, 0xff))))
//  val cliAXI = LazyModule(new ClientAXI)
//  val convCliAXI:TLOutwardNode = (TLWidthWidget(4) := AXI4ToTL() := AXI4UserYanker(capMaxFlight=Some(8)) := AXI4Fragmenter() := cliAXI.node)
//  val serr = LazyModule(new TLError(DevNullParams(Seq(AddressSet(0x400, 0xff)), 8, 128, region = RegionType.TRACKED), buffer = false, beatBytes = 4))
//  val sbar = LazyModule(new TLXbar())
//  
//  manAXI.node  := TLToAXI4() := TLWidthWidget(4) := cliTL.node 
//  manTL.node   := TLWidthWidget(4) := sbar.node
//  serr.node    := sbar.node
//  sbar.node    := cliAXI.crossTLOut(convCliAXI)
//
//  lazy val module = new LazyModuleImp(this) {
//    val io = IO(new Bundle {
//      val manAXI = new Bundle {
//        val a = Output(UInt(8.W))
//        val b = Output(UInt(8.W))
//        val c = Output(UInt(8.W))
//      }
//      val manTL = new Bundle {
//        val reg = Output(UInt(8.W))
//      }
//    })
//    io.manAXI <> manAXI.module.io
//    io.manTL.reg <> manTL.module.io
//
//  }
//}

/* A slightly more complex example with a topology as shown below:*/
class LazyAXITop(implicit p: Parameters) extends LazyModule {
/*    (GCD)
     (0x200) (0x300) (0x900)  {each AS to base + 0xff}
     manTL1  manAXI  clErr
       ^       ^       ^
       |       |       |
       -------sbar------
               ^ 
(GCD)          |
(0x100)  (0x200,0x300)   (0x800)
manTL0         cl        axiErr
  ^            ^           ^
  |            |           |
  -----------mbar-----------
  ^                        ^
  |                        | 
cliAXI                   cliTL0
*/

  //Choosing a register router for convenience; could uncomment the relevant bare bones manager*/
  val manTL1   = LazyModule(new ManagerRegTL(new ManagerRegTLParams(address = 0x200)))
  //val manTL1 = LazyModule(new ManagerTL(Seq(AddressSet(0x200, 0xff))))
  val manAXI = LazyModule(new ManagerRegAXI(AddressSet(0x300, 0xff)))
  //val manAXI= LazyModule(new ManagerAXI)
  //val convManAXI: TLInwardNode = (manAXI.node := AXI4Buffer() := AXI4UserYanker()  := AXI4Deinterleaver(64) := AXI4IdIndexer(idBits=2) := TLToAXI4())
  val clerr = LazyModule(new TLError(DevNullParams(Seq(AddressSet(0x900, 0xff)), 8, 128, region = RegionType.TRACKED), buffer = false, beatBytes = 4))
  val sbar = LazyModule(new TLXbar())
  //Can't add a client to sbar here for some reason
  //val cliTL1 = LazyModule(new ClientGCD(Seq(AddressSet(0x100, 0xff))))
  val manTL0   = LazyModule(new ManagerRegTL(new ManagerRegTLParams)) //0x100
  //val manTL0 = LazyModule(new ManagerTL(Seq(AddressSet(0x100, 0xff))))
  val cl = LazyModule(new ChipLink(ChipLinkParams(
  //TODO change atomicXfer to 1,4 instead of 1,8
    TLUH = Seq(AddressSet(0x200, 0xff), AddressSet(0x300,0xff)),
    TLC  = Seq(AddressSet(0x500, 0xff)), syncTX = true)))
  val clsink = cl.ioNode.makeSink
  val axierr = LazyModule(new TLError(DevNullParams(Seq(AddressSet(0x800, 0xff)), 8, 128, region = RegionType.UNCACHED), beatBytes = 4))
  val mbar = LazyModule(new TLXbar)
  val cliAXI = LazyModule(new ClientAXI)
  //^ don't know how to specify valid address range of operation unlike cliTL0; inturn restricted by connected managers (TL/AXI)
  //Converted client for convenience
  val convCliAXI:TLOutwardNode = (TLWidthWidget(4) := AXI4ToTL() := AXI4UserYanker(capMaxFlight=Some(8)) := AXI4Fragmenter() := AXI4IdIndexer(idBits = 2) := AXI4Buffer() := cliAXI.node)
  val cliTL0 = LazyModule(new ClientTL(Seq(AddressSet(0x100, 0xff), AddressSet(0x200,0xff), AddressSet(0x300, 0xff))))

  //manAXI.crossTLIn(convManAXI) := TLWidthWidget(4) := sbar.node //in case using the vanilla AXI manager
  manAXI.node := AXI4Buffer() := AXI4UserYanker()  := AXI4Deinterleaver(64) := AXI4IdIndexer(idBits=2) := TLToAXI4() := TLWidthWidget(4) := sbar.node 
  manTL1.node := TLWidthWidget(4)  :=  sbar.node
  clerr.node :=  sbar.node
  sbar.node    := TLBuffer() := TLAtomicAutomata() := TLFIFOFixer() := TLHintHandler() := TLBuffer() := TLWidthWidget(4) := cl.node
  //sbar.node    := cliTL1.node //doesn't work
  cl.node      := mbar.node
  manTL0.node   := TLWidthWidget(4) := mbar.node
  axierr.node    := mbar.node
  mbar.node    := TLFIFOFixer() := cliAXI.crossTLOut(convCliAXI)
  mbar.node    := cliTL0.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val manAXI = new Bundle {
        val a = Output(UInt(8.W))
        val b = Output(UInt(8.W))
        val c = Output(UInt(8.W))
      }
      val manTL0 = new Bundle {
        val x = Output(UInt(8.W))
        val y = Output(UInt(8.W))
        val gcd = Output(UInt(8.W))
      }
      val manTL1 = new Bundle {
        val x = Output(UInt(8.W))
        val y = Output(UInt(8.W))
        val gcd = Output(UInt(8.W))
      }
      val chiplink = new WideDataLayerPort(ChipLinkParams(
          TLUH = Seq(AddressSet(0x300,0xff)),
          TLC = Seq()))
    })
    io.manAXI <> manAXI.module.io
    io.manTL0 <> manTL0.module.io
    io.manTL1 <> manTL1.module.io
    io.chiplink <> clsink.bundle

  }
}

/*An example connecting TL client to TL Manager via chiplink*/
class LazyGCDTop(implicit p: Parameters) extends LazyModule {
  val man0   = LazyModule(new ManagerRegTL(new ManagerRegTLParams)) //0x100
  val man1   = LazyModule(new ManagerTL(Seq(AddressSet(0x200,0xff)))) //0x200
  val serr = LazyModule(new TLError(DevNullParams(Seq(AddressSet(0x300, 0xff)), 8, 128, region = RegionType.TRACKED), beatBytes = 4))
  val sbar   = LazyModule(new TLXbar)
  val cl = LazyModule(new ChipLink(ChipLinkParams(
    TLUH = Seq(AddressSet(0x300, 0xff)),
    TLC  = Seq(AddressSet(0x400, 0xff)), syncTX = true)))
  val clsink = cl.ioNode.makeSink
  val tlxbar = LazyModule(new TLXbar)
  val cli    = LazyModule(new ClientTL(Seq(AddressSet(0x100, 0xff), AddressSet(0x300, 0xff))))
  val cliOut = LazyModule(new ClientCL(Seq(AddressSet(0x300, 0xff))))
  private def filter(m: TLManagerParameters) = 
    // keep only managers that are locally routed
    if (m.address.exists(a => Seq(AddressSet(0x100, 0xff)).exists(_.overlaps(a)))) Some(m) 
    else None


  man0.node     := TLWidthWidget(4) := TLFIFOFixer(TLFIFOFixer.allFIFO) := TLFilter(filter) := tlxbar.node
  man1.node     := TLWidthWidget(4) := sbar.node
  serr.node     := TLWidthWidget(4) := sbar.node
  sbar.node     := TLBuffer() := TLWidthWidget(4) := cl.node
  cl.node       := TLBuffer() := TLWidthWidget(4) := tlxbar.node 
  tlxbar.node   := TLBuffer() := TLSourceShrinker(4) := cliOut.node
  tlxbar.node   := cli.node

  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val tla = Valid(UInt(8.W))
      val tld = Valid(UInt(8.W))
      val tla_cO = Valid(UInt(8.W))
      val tld_cO = Valid(UInt(8.W))
      val state = Output(UInt(5.W))
      val state_cO = Output(UInt(5.W))
      val counter = Output(UInt(8.W))
      val x = Output(UInt(8.W))
      val y = Output(UInt(8.W))
      val gcd = Output(UInt(8.W))
      val reg = Output(UInt(8.W))
      //chiplink
      //val tx_clk = Input(Clock())
      val chiplink = new WideDataLayerPort(ChipLinkParams(
          TLUH = Seq(AddressSet(0x300,0xff)),
          TLC = Seq()))
    })
    io.tla_cO  <> cliOut.module.io.tla
    io.tld_cO  <> cliOut.module.io.tld
    io.tla     <> cli.module.io.tla
    io.tld     <> cli.module.io.tld
    io.x       := man0.module.io.x
    io.y       := man0.module.io.y
    io.gcd     := man0.module.io.gcd
    io.reg     := man1.module.io
    io.counter := cli.module.io.counter
    io.state   := cli.module.io.state
    io.state_cO   := cliOut.module.io.state
    io.chiplink <> clsink.bundle
  }
}

/**************************/
/* GCD related code below */
/**************************/

class GcdInputBundle(val w: Int) extends Bundle {
  val value1 = UInt(w.W)
  val value2 = UInt(w.W)
}

class GcdOutputBundle(val w: Int) extends Bundle {
  val value1 = UInt(w.W)
  val value2 = UInt(w.W)
  val gcd    = UInt(w.W)
}

/**
  * Compute Gcd using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value input register x is then the Gcd.
  * Unless first input is zero then the Gcd is y.
  * Can handle stalls on the producer or consumer side
  */
class DecoupledGcd(width: Int) extends MultiIOModule {
  val input = IO(Flipped(Decoupled(new GcdInputBundle(width))))
  val output = IO(Decoupled(new GcdOutputBundle(width)))

  val xInitial    = Reg(UInt())
  val yInitial    = Reg(UInt())
  val x           = Reg(UInt())
  val y           = Reg(UInt())
  val busy        = RegInit(false.B)
  val resultValid = RegInit(false.B)

  input.ready := !busy
  output.valid := resultValid
  output.bits := DontCare

  when(busy)  {
    when(x > y) {
      x := x - y
    }.otherwise {
      y := y - x
    }
    when(x === 0.U || y === 0.U) {
      when(x === 0.U) {
        output.bits.gcd := y
      }.otherwise {
        output.bits.gcd := x
      }

      output.bits.value1 := xInitial
      output.bits.value2 := yInitial
      resultValid := true.B

      when(output.ready && resultValid) {
        busy := false.B
        resultValid := false.B
      }
    }
  }.otherwise {
    when(input.valid) {
      val bundle = input.deq()
      x := bundle.value1
      y := bundle.value2
      xInitial := bundle.value1
      yInitial := bundle.value2
      busy := true.B
    }
  }
}
