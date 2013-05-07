package models

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck

class SuperSecretCrypterSpec extends Specification with ScalaCheck {

  "SuperSecretCrypter" should {
    "encrypt by simply swapping lowest and highest 4 bits" in {
      val input: Byte = 0x0F
      SuperSecretCrypter.encrypt(input) must be_!==(input)
    }

    "decrypt what was encrypted" in {
      check { (input: Byte) =>
        SuperSecretCrypter.decrypt(SuperSecretCrypter.encrypt(input)) must equalTo(input)
      }
    }
  }

}
