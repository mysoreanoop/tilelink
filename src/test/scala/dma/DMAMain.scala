// See README.md for license details.

package dma

import chisel3._
import freechips.rocketchip.config.{Parameters, Config}

object DMAMain extends App {

  implicit val p: Parameters = new BaseConfig//Parameters.empty
  iotesters.Driver.execute(args, () => new ChiselTopWrapper) {
    c => new TBDMATop(c)
  }








}

