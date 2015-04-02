package forms

import com.typesafe.scalalogging.{StrictLogging => Logging}


import play.api.libs.Files
import play.api.libs.Files.TemporaryFile
import play.api.mvc.{AnyContent, MultipartFormData}
import play.api.mvc.MultipartFormData.FilePart

/**
 * @author <a href="mailto:dev@davidsoergel.com">David Soergel</a>
 */
object FormUtils extends Logging {


  def getNestedDataFromRequest(request: play.api.mvc.Request[AnyContent]): (Map[List[String], Either[String, FilePart[TemporaryFile]]]) = {
    val (normal, files) = getDataFromRequest(request)
    val flatData = flattenRepeatedFields(normal)

    val result = flatData.map({
      case (k, v) => {
        // leading periods are optional; it's easier to generate field names including them, 
        // but distinguishing with and without cases would be confusing

        val keyList = k.split("\\.").toList
        val kl = if (keyList.head.isEmpty) keyList.tail else keyList
        (kl, Left(v))
      }
    }).toMap

    val fileResult = files.map({
      case (k, v) => {
        // leading periods are optional; it's easier to generate field names including them, 
        // but distinguishing with and without cases would be confusing

        val keyList = k.split("\\.").toList
        val kl = if (keyList.head.isEmpty) keyList.tail else keyList
        (kl, Right(v))
      }
    }).toMap

    logger.trace("FORM DATA: \n        " + result.map({
      case (kk, v) => kk.mkString(".") + " = " + v
    }).mkString("\n        "))
    logger.trace("FILE DATA: \n        " + fileResult.map({
      case (kk, v) => kk.mkString(".") + " = " + v
    }).mkString("\n        "))

    result ++ fileResult
  }


  def flattenRepeatedFields(data: Map[String, Seq[String]]): Map[String, String] = {
    data.foldLeft(Map.empty[String, String]) {
      case (s, (key, values)) if key.endsWith("[]") => s ++ values.zipWithIndex.map {
        case (v, i) => (key.dropRight(2) + "[" + i + "]") -> v
      }
      case (s, (key, values)) => s + (key -> values.headOption.getOrElse(""))
    }
  }

  def getDataFromRequest(request: play.api.mvc.Request[AnyContent]): (Map[String, Seq[String]], Map[String, MultipartFormData.FilePart[Files.TemporaryFile]]) = {

    val normal: Map[String, Seq[String]] = request.body match {
      case body: play.api.mvc.AnyContent if body.asFormUrlEncoded.isDefined => body.asFormUrlEncoded.get
      case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => body.asMultipartFormData.get.asFormUrlEncoded
      case body: play.api.mvc.AnyContent if body.asJson.isDefined => fromJson(js = body.asJson.get).mapValues(Seq(_))
      case body: Map[_, _] => body.asInstanceOf[Map[String, Seq[String]]]
      case body: play.api.mvc.MultipartFormData[_] => body.asFormUrlEncoded
      case body: play.api.libs.json.JsValue => fromJson(js = body).mapValues(Seq(_))
      case _ => Map.empty[String, Seq[String]]
    }

    val files: Map[String, FilePart[TemporaryFile]] = request.body match {
      case body: play.api.mvc.AnyContent if body.asMultipartFormData.isDefined => {
        val b = body.asMultipartFormData.get
        val filesByKey = b.files.map(x => (x.key, x)).toMap
        filesByKey
      }
      case body: play.api.mvc.MultipartFormData[Files.TemporaryFile] => {
        val filesByKey = body.files.map(x => (x.key, x)).toMap
        filesByKey
      }
      case _ => Map.empty
    }


    (normal ++ request.queryString, files)
  }

  def stringData(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]):
  Option[String] = data.get(Nil).flatMap({
    case Left(s) => Some(s)
    case Right(f) => throw new RuntimeException("Can't upload a file to a string field")
  })
  
  def fileData(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]):
  Option[FilePart[Files.TemporaryFile]] = data.get(Nil).map({
    case Left(s) => throw new RuntimeException("Can't upload a string to a file field")
    case Right(f) => f
  })

  import play.api.libs.json._

  def fromJson(prefix: String = "", js: JsValue): Map[String, String] = js match {
    case JsObject(fields) => {
      fields.map {
        case (key, value) => fromJson(Option(prefix).filterNot(_.isEmpty).map(_ + ".").getOrElse("") + key, value)
      }.foldLeft(Map.empty[String, String])(_ ++ _)
    }
    case JsArray(values) => {
      values.zipWithIndex.map {
        case (value, i) => fromJson(prefix + "[" + i + "]", value)
      }.foldLeft(Map.empty[String, String])(_ ++ _)
    }
    case JsNull => Map.empty
    case _:JsUndefined => Map.empty
    case JsBoolean(value) => Map(prefix -> value.toString)
    case JsNumber(value) => Map(prefix -> value.toString)
    case JsString(value) => Map(prefix -> value.toString)
  }


}
