package models

import play.api.libs.iteratee.Enumeratee

object Encrypt {

  def encrypt: Enumeratee[Array[Byte], Array[Byte]] = {
    Enumeratee.map { input =>
      input.map { b =>
        b
      }
    }
  }

  def swapLowestAndHighest4Bits(b: Byte) = {
    val lowest4bits = b & 0x0f
    val highest = (b >> 4) & 0x0f
    val result1: Int = highest << 4
    val result2 = result1 + lowest4bits
    result2.toByte
  }

}
