package models

import play.api.libs.iteratee.Enumeratee
import play.api.Logger

object VirusScanner {

  def scan: Enumeratee[Array[Byte], Array[Byte]] = {
    Enumeratee.map { input =>
      Logger.debug("scanning " + input.length + " bytes")
      input
    }
  }

}
