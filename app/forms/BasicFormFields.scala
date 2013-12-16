package forms

import FormUtils._
import com.typesafe.scalalogging.slf4j.Logging
import edu.umass.cs.iesl.scalacommons.{StringUtils, NonemptyString}
import java.net.URL
import org.joda.time.DateTime
import play.api.libs.Files
import play.api.mvc.MultipartFormData
import play.api.mvc.MultipartFormData.FilePart
import scala.collection.{GenSet, mutable}
import org.joda.time.format.DateTimeFormatter

import StringUtils._
import scala.collection.immutable.ListMap

/*
 * Concrete form fields.  These have scalate view templates.
 */

case class TextAreaForm(override val prefill: Option[NonemptyString],
                        override val constraints: Seq[FieldConstraint[PrefillableNestedForm[NonemptyString]]] = Nil, placeholder:String = "")
  extends PrefillCanonicalConstrainedNestedForm[NonemptyString] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      TextAreaForm(stringData(data), constraints, placeholder)
  def fill(xopt: Option[NonemptyString]) = new TextAreaForm(xopt, constraints,placeholder)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[NonemptyString]]) = new TextAreaForm(prefill,
    constraints :+ c,placeholder)
  def withPlaceholder(s:String) = new TextAreaForm(prefill,
    constraints, s)
}


case class TextForm(override val prefill: Option[NonemptyString],
                    override val constraints: Seq[FieldConstraint[PrefillableNestedForm[NonemptyString]]] = Nil, placeholder:String = "")
  extends PrefillCanonicalConstrainedNestedForm[NonemptyString] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new TextForm(stringData(data), constraints, placeholder)
  def fill(xopt: Option[NonemptyString]) = new TextForm(xopt, constraints, placeholder)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[NonemptyString]]) = new TextForm(prefill,
    constraints :+ c, placeholder)
  def withPlaceholder(s:String) = new TextForm(prefill,
    constraints, s)
}

// no need for HiddenTextForm; just use TextForm in hidden mode?
// no: by using HiddenTextForm we can be sure in the code that a template won't inadvertently use the wrong mode.

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
                          options: ListMap[NonemptyString, NonemptyString],
                          override val constraints: Seq[FieldConstraint[PrefillableNestedForm[NonemptyString]]] = Nil)
  extends PrefillCanonicalConstrainedNestedForm[NonemptyString] {

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      TextSelectForm(stringData(data), options, constraints)
  def fill(xopt: Option[NonemptyString]) = new TextSelectForm(xopt, options, constraints)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[NonemptyString]]) = new TextSelectForm(prefill, options,
    constraints :+ c)
}


case class BooleanGroupForm(override val prefill: Option[GenSet[NonemptyString]],
                          options: ListMap[NonemptyString, NonemptyString],
                          override val constraints: Seq[FieldConstraint[PrefillableNestedForm[GenSet[NonemptyString]]]] = Nil)
  extends PrefillCanonicalConstrainedNestedForm[GenSet[NonemptyString]] {
  
  val fields : Iterable[FormField[Boolean]] = options.map({case (key,name) => new FormField(key,new BooleanForm(prefill.map(p=>p.contains(key))),Some(name))})

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = {
    val boundFields = fields.map(_.nestedBind(data))
    val trueFieldKeys = boundFields.filter(_.nestedGet.getOrElse(false)).map(_.key.n).toSet
    new BooleanGroupForm(Some(trueFieldKeys), options, constraints)
  }
     
  def fill(xopt: Option[GenSet[NonemptyString]]) = {
    new BooleanGroupForm(xopt, options, constraints)
  }
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[GenSet[NonemptyString]]]) = {
    new BooleanGroupForm(prefill,options, constraints :+ c)
  }

}

case class UrlForm(override val prefill: Option[URL], override val constraints:
Seq[FieldConstraint[PrefillableNestedForm[URL]]] = Nil, placeholder:String="") extends PrefillCanonicalConstrainedNestedForm[URL] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) =
    new UrlForm(stringData(data).map(n => new URL(n.s)), constraints, placeholder)
  def fill(xopt: Option[URL]) = new UrlForm(xopt, constraints, placeholder)
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[URL]]) = new UrlForm(prefill, constraints :+ c,placeholder)
  def withPlaceholder(s:String) = new UrlForm(prefill,constraints, s)
}


case class JodaDateForm(override val prefill: Option[DateTime], override val constraints:
Seq[FieldConstraint[PrefillableNestedForm[DateTime]]] = Nil)(implicit val formatter:DateTimeFormatter) extends PrefillCanonicalConstrainedNestedForm[DateTime] {
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = new
      JodaDateForm(stringData(data).map(n =>  DateTime.parse(n.s,formatter)))
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
  
  // just do this in the template
  //val prefillBool = prefill.getOrElse("false")
}


/**
 * Utility class for cases where a form is required but we don't actually have one.  It's meaningless that it's Boolean.
 * @param prefill
 * @param constraints
 */
// oops this can't be filled or bound
/*
case class EmptyForm[T](override val prefill: Option[T] = None, override val constraints:
Seq[FieldConstraint[PrefillableNestedForm[T]]] = Nil) extends PrefillCanonicalConstrainedNestedForm[T] {
  def fromString(s: String): T = throw new NotImplementedError()
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]) = this
  def fill(xopt: Option[T]) = this
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[T]]) = this
}
*/
