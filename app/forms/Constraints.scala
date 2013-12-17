package forms


import edu.umass.cs.iesl.scalacommons.NonemptyString
import edu.umass.cs.iesl.scalacommons.StringUtils._
import scala.collection.GenTraversable

case class ConstraintNotMetException(s: String = "") extends Exception(s)

// the function f returns error messages, so None indicates success.
class FieldConstraint[-T <: NestedForm[_]](errorText: String,
                                           val infoText: Option[String] = None)(f: (T => Option[Option[String]])) {
  def apply(t: T): Unit = f(t) map {
    errorDetail => throw new ConstraintNotMetException(errorText + errorDetail.map(": " + _).getOrElse(""))
  }
}


class RequiredConstraint[T <: PrefillableNestedForm[_]] extends FieldConstraint[T]("required",
  Some("*"))(t => if (t.prefill.nonEmpty) None else Some(None))


class EmailConstraint[T <: PrefillableNestedForm[NonemptyString]] extends FieldConstraint[T]("Invalid email address")(t => {
  if (t.prefill.map(s => EmailParser.parseOne(s).isDefined).getOrElse(true)) None else Some(None)
})


class MultiEmailConstraint[T <: PrefillableNestedForm[NonemptyString]] extends FieldConstraint[T]("Invalid email addresses")(t => {
  val errors = t.prefill.flatMap(s => {
    EmailParser.parseMulti(s) match {
      case Right(x) => None
      case Left(x) => Some(Some(x.mkString(", ")))
    }
  })
  errors
})


case class ParsedEmail(firstName: Option[NonemptyString], lastName: Option[NonemptyString], email: NonemptyString)

object EmailParser {
  private val emailOnlyRE = """(?i)^[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4}$""".r //http://www.regular-expressions.info/email.html 

  private val nameAndEmailRE = """(?i)^(([^@]*)\s+)?(<?([A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,4})>?)?.*$""".r // just capture the first email, if any

  private val splitName = """(.*)\s+(.*)""".r

  def parseOne(s: String): Option[ParsedEmail] = {
    try {
      val nameAndEmailRE(x, fullName, y, email) = s

      val emailOpt = if(email==null) None else email.opt
      
      // could use namejuggler; just be naive for now
      try {
        
        val splitName(firstName, lastName) = fullName

        email.opt.map(e => ParsedEmail(firstName.opt, lastName.opt, e))
      }
      catch {
        case e: MatchError if fullName == null || fullName.trim.isEmpty => emailOpt.map(e => ParsedEmail(None, None, e))
        case e: MatchError => email.opt.map(e => ParsedEmail(None, fullName.opt, e))
      }
    }
    catch {
      case e: MatchError => None
    }

  }

  // like Validation but without scalaz
  def parseMulti(s: String): Either[GenTraversable[NonemptyString], GenTraversable[ParsedEmail]] = {
    val lines = s.split("[,;\t\n]")
    val parsedLines = lines.flatMap(_.opt).map(s => (s, parseOne(s))).toMap
    val (success, failure) = parsedLines.partition(_._2.isDefined)
    if (failure.nonEmpty) Left(failure.keys)
    else Right(success.values.map(_.get))
  }

}


/**
 * This means that any required fields inside the template should actually be required.
 * Otherwise such "required" fields could still be empty because we're creating a template that won't be reified yet
 * @tparam T
 */
class TemplateCompleteConstraint[T <: PrefillableNestedForm[_]] extends FieldConstraint[T]("complete template required",
  Some("*"))(t => if (t.prefill.nonEmpty) None else Some(None))


case class FixedConstraint[X, -T <: NestedForm[_]](fixedValue: X) extends FieldConstraint[T]("must match fixed value")(form => {
  val actual = form.get
  if (actual == Some(fixedValue)) None else Some(Some(fixedValue.toString))
}
)


class InfoNoConstraint[T <: PrefillableNestedForm[_]](message: String) extends FieldConstraint[T]("bogus", Some(message))(t => None)
