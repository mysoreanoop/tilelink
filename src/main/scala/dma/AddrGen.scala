package dma

import chisel3._
import chisel3.util._
import chisel3.util.Reverse
import freechips.rocketchip.config.{Parameters, Field, Config}

class ProcBun(addrCtrlWidth: Int, addrWidth: Int) extends Bundle {
  val xCnt = UInt(addrCtrlWidth.W)
  val yCnt = UInt(addrCtrlWidth.W)
  val addr = UInt(addrWidth.W)
  
  override def cloneType = (new ProcBun(addrCtrlWidth, addrWidth)).asInstanceOf[this.type]
} 

class AddrOut(implicit p: Parameters) extends Bundle {
  val c = p(DMAKey)
  val addr = UInt(c.addrWidth.W)
  val size = UInt(log2Up(c.lgMaxBytes).W)
  val mask = UInt(c.beatBytes.W)
  val last = Bool()

  override def cloneType = (new AddrOut).asInstanceOf[this.type]
}

/* Produces address to read from or write to*/
class AddressGenerator(implicit p: Parameters) extends Module {
  val c = p(DMAKey)
  val io = IO(new Bundle {
    val cmd = Input(new PortParam)
    val start = Input(Bool())
    val out = Decoupled(new AddrOut)
  })
  val cmd = RegInit({val n = Wire(new ProcBun(c.addrCtrlWidth, c.addrWidth));
    n := DontCare; n })
  val last = calcLast(cmd.xCnt, cmd.yCnt) 
  
  io.out.valid := io.start
  when(catchRisingEdge(io.start)) {
    io.out.valid := false.B
    //when(io.req) {
    //  io.out.bits.addr := io.cmd.addr
    //  io.out.bits.size := calcSize(io.cmd.xCnt)
    //  io.out.bits.mask := calcMask(io.cmd.xCnt)
    //  io.out.bits.last := calcLast(io.cmd.xCnt, io.cmd.yCnt)
    //  cmd := calcAddr({val x = Wire(new ProcBun(c.addrCtrlWidth, c.addrWidth));
    //    x.xCnt := io.cmd.xCnt
    //    x.yCnt := io.cmd.yCnt
    //    x.addr := io.cmd.addr
    //    x
    //  })
    //} .otherwise {
      cmd.xCnt := io.cmd.xCnt
      cmd.yCnt := io.cmd.yCnt
      cmd.addr := io.cmd.addr
    //}
  } .elsewhen(io.out.fire()) {
    cmd := calcAddr(cmd)
  }
  io.out.bits.addr := cmd.addr 
  io.out.bits.size := calcSize(cmd.xCnt)
  io.out.bits.mask := calcMask(cmd.xCnt)
  io.out.bits.last := calcLast(cmd.xCnt, cmd.yCnt)

  def catchRisingEdge(n: Bool): Bool = { n && ~RegNext(n) }
  def calcSize(x: UInt): UInt = {
    val out = Wire(UInt(c.maxWords.W))
    when(x >= c.maxWords.U) {
      out := c.lgMaxBytes.U
    } .otherwise {
      out := 0.U//means 1 beat
    }
    out
  }
  def calcMask(x: UInt): UInt = {
    def createMask(n: UInt): UInt = {
      //Reverse((1.U << n) - 1.U) TODO hangs!!!
      (1.U << n) - 1.U 
    }
    val out = Wire(UInt(c.beatBytes.W))
    when(x >= c.wordsPerBeat.U) {
      out := Fill(c.beatBytes, 1.U)
    } .otherwise {
      out := createMask(x)
      //^for the remaining words that cannot make up an entire beat
    }
    out
  }
  def calcLast(x: UInt, y: UInt): Bool = {
    val out = WireInit(false.B)
    when(y === 0.U) {
      when((x === c.maxWords.U || x <= c.wordsPerBeat.U) 
          && ~catchRisingEdge(io.start)) {
        out := true.B
      }
    }
    out
  }
  def calcAddr(n: ProcBun) = {
    val out = Wire(new ProcBun(c.addrWidth, c.addrWidth))
    out := n
    when(n.xCnt > 0.U) {
      /*This exploits busWidth and burst advantages.*/
      when(n.xCnt >= c.maxWords.U) {
        when(n.xCnt === c.maxWords.U) {
          out.xCnt := Mux(n.yCnt > 0.U, io.cmd.xCnt, 0.U)
          out.yCnt := Mux(n.yCnt === 0.U, 0.U, n.yCnt - 1.U)
          out.addr := n.addr + c.maxBytes.U + io.cmd.yStep * (c.dataWidth/8).U
        } .otherwise {
          out.xCnt := n.xCnt - c.maxWords.U
          out.addr := n.addr + c.maxBytes.U
        }
      } .otherwise {
        /* Cut it up into as many single beats and one final masked beat
         * Determining single max possible burstSize is costly*/
        when(n.xCnt >= c.wordsPerBeat.U) {
          when(n.xCnt === c.wordsPerBeat.U) {
            out.xCnt := Mux(n.yCnt > 0.U, io.cmd.xCnt, 0.U)
            out.addr := n.addr + c.beatBytes.U + io.cmd.yStep * (c.dataWidth/8).U
            out.yCnt := Mux(n.yCnt === 0.U, 0.U, n.yCnt - 1.U)
          } .otherwise {
            out.xCnt := n.xCnt - c.wordsPerBeat.U
            out.addr := n.addr + c.beatBytes.U
          }
        } .otherwise {//masked beat
          out.xCnt := Mux(n.yCnt > 0.U, io.cmd.xCnt, 0.U)
          out.addr := n.addr + n.xCnt * (c.dataWidth/8).U
          out.yCnt := Mux(n.yCnt === 0.U, 0.U, n.yCnt - 1.U)
        }
      }
    } .otherwise {
      //don't come here!
    }
    out
  }
}

