package models

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import play.api.libs.iteratee.{Iteratee, Enumerator}
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class EncryptStreamSpec extends Specification with ScalaCheck {

  "EncryptStream" should {
    "encrypt an stream" in {
      prop { (source: String) =>
        (source.length != 0) ==> {
          val sourceAsByte = source.getBytes
          val encryptedStream = Enumerator(sourceAsByte) through EncryptStream.encrypt
          val encryptedBytes = joinValues(encryptedStream).head
          encryptedBytes must be_!==(sourceAsByte)
        }
      }
    }
    "decrypt an encrypted an stream" in {
      prop { (source: String) =>
        val sourceAsByte = source.getBytes
        val stream = Enumerator(sourceAsByte) through EncryptStream.encrypt through EncryptStream.decrypt
        val bytes = joinValues(stream).head
        bytes must beEqualTo(sourceAsByte)
      }
    }
  }

  private def joinValues[A](values: Enumerator[A]): List[A] = {
    val join = Iteratee.fold[A, List[A]](Nil)((list, el) => el :: list)
    Await.result(Iteratee.flatten(values |>> join).run, Duration.Inf).reverse
    // Iteratee.consume[Array[Byte]]()
  }

}
