package models

import play.api.libs.iteratee._
import play.api.Logger

object SafeProcess {

  def upload: Enumeratee[Array[Byte], Either[VirusFound, Array[Byte]]] = {
    EncryptStream.encrypt compose VirusScanner.scan
  }

  def uploadProcess: Iteratee[Array[Byte], Array[Byte]] = for {
    encrypt <- encrypt
    value <- stopOnVirus
  } yield value

  def stopOnVirus: Iteratee[Array[Byte], Array[Byte]] = {
    checkForVirus.flatMap {
      case Right(b) => Done(b)
      case Left(virusFound) => {
        Logger.warn("found virus: " + virusFound.virusName)
        Error(virusFound.virusName, Input.Empty)
      }
    }
  }

  def checkForVirus: Iteratee[Array[Byte], Either[VirusFound, Array[Byte]]] = Cont {
    case in @ Input.El(char) => {
      val s = new String(char)
      if (s.contains("I love You")) {
        Done(Left(VirusFound("I love You")), in)
      } else {
        Done(Right(char), in)
      }
    }
    case in @ Input.EOF => checkForVirus
    case Input.Empty => checkForVirus
  }

  def encrypt: Iteratee[Array[Byte], Array[Byte]] = Cont {
    case in @ Input.El(bytes) => Done(bytes.map(SuperSecretCrypter.encrypt(_)))
    case in @ Input.EOF => encrypt
    case Input.Empty    => encrypt
  }

  def download: Enumeratee[Array[Byte], Either[VirusFound, Array[Byte]]] = {
    EncryptStream.decrypt compose VirusScanner.scan
  }

}
