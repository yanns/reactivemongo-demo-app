package controllers

import scala.concurrent.{ExecutionContext, Future}

import models.{VirusFound, SafeProcess, EncryptStream, Article}
import models.Article._
import play.api.mvc._
import play.modules.reactivemongo.MongoController
import reactivemongo.api.gridfs.{ReadFile, DefaultFileToSave, GridFS}
import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
import reactivemongo.bson._
import reactivemongo.api.Cursor
import play.api.libs.iteratee.{Enumeratee, Iteratee}

object Safe extends Controller with MongoController {
  // get the collection 'articles'
  val collection: reactivemongo.api.collections.default.BSONCollection = db("articles")
  // a GridFS store named 'attachments'
  val gridFS = new GridFS(db, "attachments")

  // save the uploaded file as an attachment of the article with the given id
  def saveAttachment(id: String) = Action(uploadSafeParser(gridFS)) { request =>
    // first, get the attachment matching the given id, and get the first result (if any)
    val uploaded = collection.find(BSONDocument("_id" -> new BSONObjectID(id))).one[Article]

    val futureUpload = for {
      // we filter the future to get it successful only if there is a matching Article
      article <- uploaded.filter(_.isDefined).map(_.get)
      // we wait (non-blocking) for the upload to complete.
      putResult <- request.body.files.head.ref
      // when the upload is complete, we add the article id to the file entry (in order to find the attachments of the article)
      result <- gridFS.files.update(BSONDocument("_id" -> putResult.id), BSONDocument("$set" -> BSONDocument("article" -> article.id.get)))
    } yield result

    Async {
      futureUpload.map {
        case _ => Redirect(routes.Articles.showEditForm(id))
      }.recover {
        case _ => BadRequest
      }
    }
  }

  /** Gets a body parser that will save a file sent with multipart/form-data into the given GridFS store. */
  def uploadSafeParser[Structure, Reader[_], Writer[_]](gfs: GridFS[Structure, Reader, Writer])(implicit readFileReader: Reader[ReadFile[BSONValue]], ec: ExecutionContext): BodyParser[MultipartFormData[Future[ReadFile[BSONValue]]]] = {
    import BodyParsers.parse._

    multipartFormData(Multipart.handleFilePart {
      case Multipart.FileInfo(partName, filename, contentType) => {
        for {
          _ <- SafeProcess.stopOnVirus
          b <- EncryptStream.encrypt.transform(gfs.iteratee(DefaultFileToSave(filename, contentType)))
        } yield b
      }
    })
  }


  def getAttachment(id: String) = Action {
    Async {
      // find the matching attachment, if any, and streams it to the client
      val file = gridFS.find(BSONDocument("_id" -> new BSONObjectID(id)))
      serveSafe(gridFS, file)
    }
  }

  def serveSafe[T <: ReadFile[_ <: BSONValue], Structure, Reader[_], Writer[_]](gfs: GridFS[Structure, Reader, Writer], foundFile: Cursor[T], dispositionMode: String = CONTENT_DISPOSITION_ATTACHMENT)(implicit ec: ExecutionContext): Future[Result] = {
    foundFile.headOption.filter(_.isDefined).map(_.get).map { file =>
      val en = gfs.enumerate(file).through(EncryptStream.decrypt)
      val filename = file.filename
      SimpleResult(
        // prepare the header
        header = ResponseHeader(OK, Map(
          CONTENT_LENGTH -> ("" + file.length),
          CONTENT_DISPOSITION -> (s"""$dispositionMode; filename="$filename"; filename*=UTF-8''""" + java.net.URLEncoder.encode(filename, "UTF-8").replace("+", "%20")),
          CONTENT_TYPE -> file.contentType.getOrElse("application/octet-stream"))),
        // give Play this file enumerator
        body = en)
    }.recover {
      case _ => NotFound
    }
  }

}