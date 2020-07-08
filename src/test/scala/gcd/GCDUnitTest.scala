// See README.md for license details.

package gcd

import java.io.File

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.LazyModule

class TBLazyTop(c:ChiselTopWrapper) extends PeekPokeTester(c) {
  println("Running TBLazyTop!")
  step(100)
}
