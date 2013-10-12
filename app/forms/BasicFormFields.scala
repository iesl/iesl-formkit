package forms

import FormUtils._
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.scalacommons.NonemptyString
import java.net.URL
import org.joda.time.DateTime
import play.api.libs.Files
import play.api.mvc.MultipartFormData
import play.api.mvc.MultipartFormData.FilePart
import scala.collection.mutable

/*
 * Concrete form fields.  These have scalate view templates.
 */

case class TextAreaForm(override val prefill: Option[NonemptyString],
                        override val constraints: Seq[FieldConstraint[PrefillableNestedForm[NonemptyString]]] = Nil)
  extends PrefillCanonicalConstrainedNestedForm[NonemptyString] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      TextAreaForm(stringData(data), constraints)

  def fill(xopt: Option[NonemptyString]) = new TextAreaForm(xopt, constraints)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[NonemptyString]]) = new TextAreaForm(prefill,
    constraints :+ c)
}


case class TextForm(override val prefill: Option[NonemptyString],
                    override val constraints: Seq[FieldConstraint[PrefillableNestedForm[NonemptyString]]] = Nil)
  extends PrefillCanonicalConstrainedNestedForm[NonemptyString] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new TextForm(stringData(data), constraints)
  def fill(xopt: Option[NonemptyString]) = new TextForm(xopt, constraints)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[NonemptyString]]) = new TextForm(prefill,
    constraints :+ c)
}


case class HiddenTextForm(override val prefill: Option[NonemptyString],
                          override val constraints: Seq[FieldConstraint[PrefillableNestedForm[NonemptyString]]] =
                          Nil) extends PrefillCanonicalConstrainedNestedForm[NonemptyString] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      HiddenTextForm(stringData(data), constraints)
  def fill(xopt: Option[NonemptyString]) = new HiddenTextForm(xopt, constraints)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[NonemptyString]]) = new HiddenTextForm(prefill,
    constraints :+ c)
}


case class FileUploadForm(override val prefill: Option[FilePart[Files.TemporaryFile]] = None,
                          override val constraints: Seq[FieldConstraint[PrefillableNestedForm[FilePart[Files
                          .TemporaryFile]]]] = Nil) extends PrefillCanonicalConstrainedNestedForm[FilePart[Files.TemporaryFile]] with
Logging {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      FileUploadForm(fileData(data), constraints)
  def fill(xopt: Option[FilePart[Files.TemporaryFile]]) = new FileUploadForm(xopt, constraints)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[FilePart[Files.TemporaryFile]]]) =
    new FileUploadForm(prefill, constraints :+ c)
}


case class TextSelectForm(override val prefill: Option[NonemptyString],
                          options: mutable.LinkedHashMap[NonemptyString, NonemptyString],
                          override val constraints: Seq[FieldConstraint[PrefillableNestedForm[NonemptyString]]] = Nil)
  extends PrefillCanonicalConstrainedNestedForm[NonemptyString] {

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      TextSelectForm(stringData(data), options, constraints)
  def fill(xopt: Option[NonemptyString]) = new TextSelectForm(xopt, options, constraints)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[NonemptyString]]) = new HiddenTextForm(prefill,
    constraints :+ c)
}


case class UrlForm(override val prefill: Option[URL], override val constraints:
Seq[FieldConstraint[PrefillableNestedForm[URL]]] = Nil) extends PrefillCanonicalConstrainedNestedForm[URL] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new UrlForm(stringData(data).map(n => new URL(n.s)), constraints)
  def fill(xopt: Option[URL]) = new UrlForm(xopt, constraints)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[URL]]) = new UrlForm(prefill, constraints :+ c)
}


case class JodaDateForm(override val prefill: Option[DateTime], override val constraints:
Seq[FieldConstraint[PrefillableNestedForm[DateTime]]] = Nil) extends PrefillCanonicalConstrainedNestedForm[DateTime] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      JodaDateForm(stringData(data).map(n => new DateTime(n.s)))
  def fill(xopt: Option[DateTime]) = new JodaDateForm(xopt, constraints)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[DateTime]]) = new JodaDateForm(prefill, constraints :+ c)
}


case class BooleanForm(override val prefill: Option[Boolean], override val constraints:
Seq[FieldConstraint[PrefillableNestedForm[Boolean]]] = Nil) extends PrefillCanonicalConstrainedNestedForm[Boolean] {
  def fromString(s: String): Boolean = s match {
    case "on" => true
    case "off" => false
    case "" => false
    case x => try {
      x.toBoolean
    } catch {
      case e: NumberFormatException => false
    }
  }

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      BooleanForm(stringData(data).map(ne => fromString(ne.s)))
  def fill(xopt: Option[Boolean]) = new BooleanForm(xopt, constraints)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[Boolean]]) = new BooleanForm(prefill, constraints :+ c)
}
