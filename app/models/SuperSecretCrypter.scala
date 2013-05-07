package models

object SuperSecretCrypter {

  def encrypt(b: Byte): Byte = swapLowestAndHighest4Bits(b)
  def decrypt(b: Byte): Byte = swapLowestAndHighest4Bits(b)

  private def swapLowestAndHighest4Bits(b: Byte): Byte = {
    val lowest4bits = b & 0x0f
    val highest = (b >> 4) & 0x0f
    val result1: Int = lowest4bits << 4
    val result2 = result1 + highest
    result2.toByte
  }

}
