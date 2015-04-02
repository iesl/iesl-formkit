package forms

import FormUtils._
import com.typesafe.scalalogging.{StrictLogging => Logging}

import java.net.URL
import org.joda.time.DateTime
import play.api.libs.Files
import play.api.mvc.MultipartFormData
import play.api.mvc.MultipartFormData.FilePart
import scala.collection.Set
import org.joda.time.format.DateTimeFormatter

import scala.collection.immutable.ListMap

/*
 * Concrete form fields.  These have scalate view templates.
 */

case class TextAreaForm(
  override val prefill: Option[String],
  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[String]]] = Nil, placeholder: String = ""
)  extends PrefillCanonicalConstrainedNestedForm[String] {

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new TextAreaForm(stringData(data), constraints, placeholder)

  def fill(xopt: Option[String]) =
    new TextAreaForm(xopt, constraints, placeholder)

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[String]]) =
    new TextAreaForm(prefill, constraints :+ c, placeholder)

  def withPlaceholder(s: String) =
    new TextAreaForm(prefill, constraints, s)
}


case class TextForm(
  override val prefill: Option[String],
  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[String]]] = Nil,
  placeholder: String = ""
) extends PrefillCanonicalConstrainedNestedForm[String] {

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new TextForm(stringData(data), constraints, placeholder)

  def fill(xopt: Option[String]) =
    new TextForm(xopt, constraints, placeholder)

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[String]]) =
    new TextForm(prefill, constraints :+ c, placeholder)

  def withPlaceholder(s: String) =
    new TextForm(prefill, constraints, s)
}

// no need for HiddenTextForm; just use TextForm in hidden mode?
// no: by using HiddenTextForm we can be sure in the code that a template won't inadvertently use the wrong mode.
case class HiddenTextForm(
  override val prefill: Option[String],
  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[String]]] = Nil
) extends PrefillCanonicalConstrainedNestedForm[String] {

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new HiddenTextForm(stringData(data), constraints)

  def fill(xopt: Option[String]) =
    new HiddenTextForm(xopt, constraints)

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[String]]) =
    new HiddenTextForm(prefill, constraints :+ c)
}


case class FileUploadForm(
  override val prefill: Option[FilePart[Files.TemporaryFile]] = None,
  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[FilePart[Files.TemporaryFile]]]] = Nil
) extends PrefillCanonicalConstrainedNestedForm[FilePart[Files.TemporaryFile]] with Logging {

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new FileUploadForm(fileData(data), constraints)

  def fill(xopt: Option[FilePart[Files.TemporaryFile]]) =
    new FileUploadForm(xopt, constraints)

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[FilePart[Files.TemporaryFile]]]) =
    new FileUploadForm(prefill, constraints :+ c)
}


case class TextSelectForm(
  override val prefill: Option[String],
  options: ListMap[String, String],
  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[String]]] = Nil
) extends PrefillCanonicalConstrainedNestedForm[String] {

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new TextSelectForm(stringData(data), options, constraints)

  def fill(xopt: Option[String]) =
    new TextSelectForm(xopt, options, constraints)

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[String]]) =
    new TextSelectForm(prefill, options, constraints :+ c)
}


case class BooleanGroupForm(override val prefill: Option[Set[String]],
                            options: ListMap[String, String],
                            override val constraints: Seq[FieldConstraint[PrefillableNestedForm[Set[String]]]] = Nil)
  extends PrefillCanonicalConstrainedNestedForm[Set[String]] {

  val fields: Iterable[FormField[Boolean]] = options.map({
    case (key, name) => new FormField(key, new BooleanForm(prefill.map(p => p.contains(key))), Some(name))
  })

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = {
    val boundFields = fields.map(_.nestedBind(data))
    val trueFieldKeys = boundFields.filter(_.nestedGet.getOrElse(false)).map(_.key).toSet
    new BooleanGroupForm(Some(trueFieldKeys), options, constraints)
  }

  def fill(xopt: Option[Set[String]]) = {
    new BooleanGroupForm(xopt, options, constraints)
  }

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[Set[String]]]) = {
    new BooleanGroupForm(prefill, options, constraints :+ c)
  }

}

case class UrlForm(
  override val prefill: Option[URL],
  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[URL]]] = Nil,
  placeholder: String = ""
) extends PrefillCanonicalConstrainedNestedForm[URL] {

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new UrlForm(stringData(data).map(n => new URL(n)), constraints, placeholder)

  def fill(xopt: Option[URL]) = new UrlForm(xopt, constraints, placeholder)

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[URL]]) = new UrlForm(prefill, constraints :+ c, placeholder)

  def withPlaceholder(s: String) = new UrlForm(prefill, constraints, s)
}


case class JodaDateForm(
  override val prefill: Option[DateTime],
  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[DateTime]]] = Nil
)(implicit val formatter: DateTimeFormatter) extends PrefillCanonicalConstrainedNestedForm[DateTime] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      JodaDateForm(stringData(data).map(n => DateTime.parse(n, formatter)))

  def fill(xopt: Option[DateTime]) = new JodaDateForm(xopt, constraints)

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[DateTime]]) = new JodaDateForm(prefill, constraints :+ c)
}


/**
 * combine a date picker with an hour picker.  For now this is clearer for our purposes than a full-fledged javascript
 * date-and-time picker, but it would be easy to swap out.
 */
case class JodaDateAndHourForm(
  override val prefill: Option[DateTime],
  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[DateTime]]] = Nil
)(
  implicit val formatter: DateTimeFormatter
) extends PrefillCanonicalConstrainedNestedForm[DateTime] {

  // midnight could be a special case:
  // users might want to think of it in terms of the end of the previous day, not the start of the next day.
  // oh well, it's easier to just call it (correctly) hour 0, so the template may need explanatory text.
  
  private val timeOptions = ListMap[String, String]("0" -> "Midnight (start of day)",
    "1" -> "1 AM",
    "2" -> "2 AM",
    "3" -> "3 AM",
    "4" -> "4 AM",
    "5" -> "5 AM",
    "6" -> "6 AM",
    "7" -> "7 AM",
    "8" -> "8 AM",
    "9" -> "9 AM",
    "10" -> "10 AM",
    "11" -> "11 AM",
    "12" -> "Noon",
    "13" -> "1 PM",
    "14" -> "2 PM",
    "15" -> "3 PM",
    "16" -> "4 PM",
    "17" -> "5 PM",
    "18" -> "6 PM",
    "19" -> "7 PM",
    "20" -> "8 PM",
    "21" -> "9 PM",
    "22" -> "10 PM",
    "23" -> "11 PM")

  val date = FormField("date", JodaDateForm(prefill), Some("Date"))
  val hour = FormField("hour", TextSelectForm(Some(prefill.get.getHourOfDay.toString), timeOptions), Some("Time"))

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = {
    val dO = date.nestedBind(data)
    val hO = hour.nestedBind(data)

    // here's where we'd have to implement midnight special-casing
    val updated : Option[DateTime] = dO.nestedGet.map(d=>d.withHourOfDay(hO.nestedGet.map(h=>h.toInt).getOrElse(24)))
    
    new JodaDateAndHourForm(updated)
  }

  def fill(xopt: Option[DateTime]) = new JodaDateAndHourForm(xopt, constraints)

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[DateTime]]) = new JodaDateAndHourForm(prefill, constraints :+ c)
}

case class BooleanForm(
  override val prefill: Option[Boolean],
  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[Boolean]]] = Nil
) extends PrefillCanonicalConstrainedNestedForm[Boolean] {

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

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new BooleanForm(stringData(data).map(ne => fromString(ne)))

  def fill(xopt: Option[Boolean]) =
    new BooleanForm(xopt, constraints)

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[Boolean]]) =
    new BooleanForm(prefill, constraints :+ c)

  // just do this in the template
  //val prefillBool = prefill.getOrElse("false")
}


