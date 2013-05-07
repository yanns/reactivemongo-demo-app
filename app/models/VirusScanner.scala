package models

import play.api.libs.iteratee.{Input, Enumeratee}
import play.api.Logger

case class VirusFound(virusName: String)

object VirusScanner {

  def scan: Enumeratee[Array[Byte], Either[VirusFound, Array[Byte]]] = {
    Enumeratee.mapInput { input: Input[Array[Byte]] =>
      input match {
        case other => other.map(e => Right(e))
      }

    }
  }

}
