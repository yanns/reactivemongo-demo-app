package models

import org.specs2.mutable.Specification

class EncryptTest extends Specification {

  "Encrypt" should {
    "swap lowest and highest 4 bits" in {
      val input: Byte = 0x0F
      val result: Byte = 0x0F
      println(input)
      Encrypt.swapLowestAndHighest4Bits(input) must equalTo(result)
    }
  }

}
