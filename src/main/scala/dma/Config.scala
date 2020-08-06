package dma

import chisel3._
import freechips.rocketchip.config.{Parameters, Field, Config}

class BaseConfig extends Config(
  new WithDMA(true) ++ 
  new WithDSM(true))

