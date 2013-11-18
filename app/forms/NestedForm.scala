package forms

import play.api.libs.Files
import play.api.mvc.MultipartFormData

trait GenericNestedForm {
  def errors: Seq[String]
  def hasErrors: Boolean
}


/*
 * These are obsolete but still possibly useful
 *
 * @tparam F what can you fill it with
 * @tparam R what can you get out of it
 * @tparam C what constraints can be applied to it
 */

// 

trait NestedForm[+F] extends GenericNestedForm {
  self =>

  /**
   * Receive form data in the form of a map; repackage it as a modified copy of this.
   * The modified copy may be redisplayed to the user in the case of a validation error,
   * or can be queried for a valid model object.
   * @param data
   * @return
   */
  def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]): NestedForm[F]

  /**
   * Holds the current value of the underlying model object.
   * This may not be a valid object, in that it may violate form-level constraints.
   * For nested forms of dynamic type, this also drives the selection of which subform to instantiate.
   * @see get
   * @return
   */
  def prefill: Option[F]

  /**
   * Performs validations and returns a valid model object if possible.
   * None may be a legitimate value of the form, unless it also has RequiredConstraint,
   * in which case it will either return Some or throw an exception.
   * @throws lib.ConstraintNotMetException
   * @see prefill
   * @return
   */
  @throws(classOf[ConstraintNotMetException])
  def get: Option[F]

  def errors: Seq[String] = Nil

  def constraintInfos: Seq[String] = Nil

  def hasErrors: Boolean = errors.nonEmpty

  def isEmpty: Boolean = prefill.isEmpty
}


trait GenericConstrainedNestedForm extends GenericNestedForm {
  def fixedValue: Option[_]
}


trait ConstrainedNestedForm[F] extends GenericConstrainedNestedForm with PrefillableNestedForm[F] {

  self =>


  override val prefill: Option[F]


  // find an exact value which this field must have, as represented by a FixedConstraint.
  // This mostly makes sense for fields of primitive types, e.g. String.  If the field has subfields, then
  // fixing the value will make _all_ of those uneditable.
  // if the field has subfields and we want to fix some of those: oh argh.
  def fixedValue: Option[F] = {
    // error if the FixedConstraint is of the wrong type
    val fcOpt = constraints.find(c => c.isInstanceOf[FixedConstraint[F, _]])
    val fixedConstraint = fcOpt.map(_.asInstanceOf[FixedConstraint[F, PrefillableNestedForm[F]]])
    fixedConstraint.map(_.fixedValue)
  }

  override def bind(data: Map[List[String], Either[String, MultipartFormData.FilePart[Files.TemporaryFile]]]):
  ConstrainedNestedForm[F]


  /**
   * Returns a copy of this form with the given constraint added.  Note the constraint will not actually be applied 
   * until get() is called.
   * @param c
   * @return
   */
  def withConstraint(c: FieldConstraint[PrefillableNestedForm[F]]): ConstrainedNestedForm[F]

  /**
   * Convenience method to add a required constraint.
   * @param isRequired
   * @return
   */
  def required(isRequired: Boolean = true): ConstrainedNestedForm[F] = if (isRequired) withConstraint(new
      RequiredConstraint[PrefillableNestedForm[F]])
  else this
  
  def withMessage(message:String): ConstrainedNestedForm[F] = withConstraint(new InfoNoConstraint[PrefillableNestedForm[F]](message))
}


trait PrefillableNestedForm[F] extends NestedForm[F] {
  val prefill: Option[F]


  /**
   * Returns a new instance of this form with the underlying model object replaced.  The point of using this instead 
   * of the standard constructor is that we may want to fill() a form whose concrete type we do not know.  Also, 
   * fill() should preserve constraints and other local data, so that an existing form can be updated with new 
   * contents without worrying about those.
   * @param xopt
   * @return
   */
  def fill(xopt: Option[F]): NestedForm[F]

  val constraints: Seq[FieldConstraint[PrefillableNestedForm[F]]] = Nil

  /**
   * short-circuit fail on a single constraint exception
   * @throws lib.ConstraintNotMetException
   */
  @throws(classOf[ConstraintNotMetException])
  def applyConstraints() {
    for (c <- constraints) c.apply(this)
  }

  /**
   * Collect all constraint exceptions.
   */
  override def errors: Seq[String] = {
    val x = for (c <- constraints) yield {
      try {
        c.apply(this); None
      } catch {
        case ConstraintNotMetException(name) => Some(name)
      }
    }
    x.flatten
  }

  /**
   * Collect all constraint descriptions
   */
  override def constraintInfos: Seq[String] = constraints.map(_.infoText).flatten
}

/**
 * A form with the property that the incoming form data can always be bound into the prefill.
 * Consequently the form contents can be represented by updating the prefill; we don't need a separate cache of data failing validation.
 * Commonly used for basic fields (e.g., a text field that accepts any string).
 * @tparam F
 */
trait PrefillCanonicalConstrainedNestedForm[F]  extends ConstrainedNestedForm[F] {
  // ** This approach is bogus.  The trait should be eliminated, and the subclasses should implement bind() and get() properly.
  
  @throws(classOf[ConstraintNotMetException])
  def get: Option[F] = {
    applyConstraints()
    prefill
  }

}
