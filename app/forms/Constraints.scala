package forms


import edu.umass.cs.iesl.scalacommons.NonemptyString

case class ConstraintNotMetException(s: String = "") extends Exception(s)

class FieldConstraint[-T <: NestedForm[_]](errorText: String,
                                           args: Seq[Any],
                                           val infoText: Option[String] = None)(f: (T => Boolean)) {
  def apply(t: T): Unit = if (!f(t)) throw new ConstraintNotMetException(errorText)
}


class RequiredConstraint[T <: PrefillableNestedForm[_]] extends FieldConstraint[T]("required", Nil,
  Some("*"))(t => t.prefill.nonEmpty)


class EmailConstraint[T <: PrefillableNestedForm[NonemptyString]] extends FieldConstraint[T]("Invalid email address", Nil)(t => t.prefill.flatMap(s => EmailConstraint.emailRE.findFirstIn(s.s)).isDefined)

  
object EmailConstraint {
  val emailRE = """(?i)^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$""".r    //http://www.regular-expressions.info/email.html 
}


/**
 * This means that any required fields inside the template should actually be required.
 * Otherwise such "required" fields could still be empty because we're creating a template that won't be reified yet
 * @tparam T
 */
class TemplateCompleteConstraint[T <: PrefillableNestedForm[_]] extends FieldConstraint[T]("complete template " +
                                                                                           "required", Nil,
  Some("*"))(t => t.prefill.nonEmpty)


case class FixedConstraint[X, -T <: NestedForm[_]](fixedValue: X) extends FieldConstraint[T]("fixed value",
  Nil)(form => {
  val actual = form.get
  actual == Some(fixedValue)
}
)



class InfoNoConstraint[T <: PrefillableNestedForm[_]](message:String) extends FieldConstraint[T]("bogus", Nil,
  Some(message))(t =>true)
