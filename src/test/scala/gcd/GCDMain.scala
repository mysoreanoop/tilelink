// See README.md for license details.

package gcd

import chisel3._
import freechips.rocketchip.config.Parameters

object GCDMain extends App {
  implicit val p: Parameters = Parameters.empty
  iotesters.Driver.execute(args, () => new ChiselTopWrapper(p)) {
    c => new TBLazyTop(c)
  }
}

