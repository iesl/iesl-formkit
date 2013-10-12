package forms

import play.api.libs.Files
import play.api.mvc.MultipartFormData

/**
 * A "form field" wraps a nested form and gives it a key (for programmatic use in generated html, json, 
 * etc.) and a display name.
 */

trait GenericFormField {
  def key: String
  def displayName: String
  def form: GenericNestedForm
}

case class FormField[F](key: String, form: NestedForm[F], explicitDisplayName: Option[String] = None) extends
GenericFormField {

  /**
   * Return the validated value of the wrapped form (in the form of a model object).
   * @throws forms.ConstraintNotMetException
   * @return
   */
  @throws(classOf[ConstraintNotMetException])
  def nestedGet: Option[F] = form.get

  def displayName = explicitDisplayName.getOrElse(key)

  /**
   * Recursively bind incoming data to this form field.  That is: from the incoming data, 
   * select all those items whose key path prefix matches the key of this field.  Lop off the head of those key 
   * paths, and then bind the resulting "subdata" to the contained form.
   * @param data
   * @return
   */
  def nestedBind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]): FormField[F] = {
    val subdata = data.collect({
      case (k, v) if (k.head == key) => (k.tail, v)
    })
    FormField(key, form.bind(subdata), explicitDisplayName)
  }
}

trait GenericRepeatedFormField {
  def name: String
  def displayName: String
  def forms: Seq[GenericNestedForm]
  def errors: Seq[String]
  def hasErrors: Boolean
  def formfactory: () => GenericNestedForm
  def constraintInfos: Seq[String]
}

case class RepeatedFormField[F](name: String, forms: Seq[NestedForm[F]], formfactory: () => NestedForm[F],
                                minimum: Int, explicitDisplayName: Option[String] = None) extends
GenericRepeatedFormField {
  @throws(classOf[ConstraintNotMetException])
  def get: Seq[F] = forms.flatMap(_.get)
  def displayName = explicitDisplayName.getOrElse(name)

  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]):
  RepeatedFormField[F] = {

    val subdata = data.collect({
      case (k, v) if (k.head == name) => (k.tail, v)
    })

    val ids = subdata.keys.toSeq.map(_.head).distinct.sortBy(_.toInt) // retain order

    val subforms = for (id <- ids) yield {
      val idata = subdata.collect({
        case (k, v) if (k.head == id) => (k.tail, v)
      })
      val f = formfactory().bind(idata)
      if (f.isEmpty) None else Some(f)
    }
    new RepeatedFormField[F](name, subforms.flatten, formfactory, minimum, explicitDisplayName)
  }

  def hasErrors: Boolean = forms.map(_.hasErrors).contains(true) || (forms.size < minimum)

  def errors: Seq[String] = Seq(if (forms.size < minimum) ("At least " + minimum + " required") else "")

  def constraintInfos: Seq[String] = Nil //if (minimum > 0) Seq("At least " + minimum + " required") else Nil
}
