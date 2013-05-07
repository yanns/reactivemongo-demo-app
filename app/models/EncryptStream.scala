package models

import play.api.libs.iteratee.Enumeratee

object EncryptStream {

  def encrypt: Enumeratee[Array[Byte], Array[Byte]] = {
    Enumeratee.map {
      _.map(SuperSecretCrypter.encrypt(_))
    }
  }

  def decrypt: Enumeratee[Array[Byte], Array[Byte]] = {
    Enumeratee.map {
      _.map(SuperSecretCrypter.decrypt(_))
    }
  }

}
