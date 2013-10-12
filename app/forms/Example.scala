package forms

import edu.umass.cs.iesl.scalacommons.NonemptyString
import java.net.URL
import org.joda.time.DateTime
import play.api.libs.Files
import play.api.libs.Files.TemporaryFile
import play.api.mvc.MultipartFormData.FilePart

/**
 * A model type which we want to edit on a form.
 * 
 * @param name
 * @param url
 * @param date
 * @param yesno
 * @param tempFile
 */
case class ExampleData(
                        name: NonemptyString,
                        url: URL,
                        date: DateTime,
                        yesno: Boolean,
                        tempFile: Option[FilePart[Files.TemporaryFile]])

/**
 * A form for editing a model object of a certain type.  This will have a matching scalate view template.
 * @param prefill
 * @param constraints
 */
class ExampleForm(override val prefill: Option[ExampleData] = None,
                  override val constraints: Seq[FieldConstraint[PrefillableNestedForm[ExampleData]]] = Nil)
                  extends ConstrainedNestedForm[ExampleData] {

  self =>

  /* Define the form fields.
   *
   * For each field, we have to explicitly choose the type of the contained form, because
   * a) prefill may be None, so we can't match on its contents, and 
   * b) a given datatype may be representable by different field types (e.g., input vs. textarea).
   * 
   * Note also how each field must be prefilled with data extracted from this form's prefill.
   * 
   * These fields can be displayed in the scalate template simply with view(name) etc.  
   * // careful: viewWithArgs, prefix etc.
   */

  val name = FormField("name", TextForm(prefill.map(_.name)))
  val url = FormField("url", UrlForm(prefill.map(_.url)))
  val date = FormField("date", JodaDateForm(prefill.map(_.date)))
  val yesno = FormField("yesno", BooleanForm(prefill.map(_.yesno)))
  val tempFile = FormField("tempFile", FileUploadForm(prefill.flatMap(_.tempFile)))

  def allFields = List(name,url,date,yesno,tempFile) // support "collection(allFields)" in jade template

  /* Recursively bind data to those fields. 
   * 
   * The issue here is that the incoming data is always in the form of Strings, which must be parsed into the model
   * object-- but it may not be parseable as the right type (e.g., "phone number" or "email address").  In this case
   * we'll end up throwing a validation error, but we'll want to prefill the returned form with the invalid data.  We
   * have to store this invalid data somewhere, but we can't put it in the model object since it doesn't fit there.
   * 
   * So: the approach is to create a new form with no prefill object, but to simply bind the incoming data back into
   * each field directly.
   * 
   * Even that binding may fail (e.g., with MalformedUrlException, or whatever); I don't think we deal with that
   * properly yet.
   */

  override def bind(data: Map[List[String], Either[String, FilePart[TemporaryFile]]]): ExampleForm =
    new ExampleForm(None, constraints) {

      override val name = self.name.nestedBind(data)
      override val url = self.url.nestedBind(data)
      override val date = self.date.nestedBind(data)
      override val yesno = self.yesno.nestedBind(data)
      override val tempFile = self.tempFile.nestedBind(data)
    }

  /**
   * Extract data from the subforms/fields and put it in a container of the right type for this form.
   * For a form consisting of multiple fields, it's rare that None would be a valid response.
   * @return
   */
  def get: Some[ExampleData] = {
    applyConstraints()
    
    try {
      // note this process validates that the data fits in ExampleData, which requires values for some fields, 
      // so if one of these nestedGet.get calls fails then that's really a validation error for a required field 
      // (and that field should have had a required constraint on it in the first place).
      Some(new ExampleData(name.nestedGet.get,url.nestedGet.get,date.nestedGet.get,yesno.nestedGet.get,tempFile.nestedGet))
    }
    catch {
      case e : ConstraintNotMetException => throw e
      case e : Throwable => throw new ConstraintNotMetException("Form error, probably suggesting insufficient field constraints.")
    }
  }
  
  /*
   * Provide the required modified-copy constructors.
   * These can't be easily generified because we don't know the constructor arguments of concrete subclasses.
   */

  def withConstraint(c: FieldConstraint[PrefillableNestedForm[ExampleData]]): ExampleForm = new ExampleForm(prefill, constraints :+ c)
  
  def fill(xopt: Option[ExampleData]): ExampleForm = new ExampleForm(xopt, constraints)
}
