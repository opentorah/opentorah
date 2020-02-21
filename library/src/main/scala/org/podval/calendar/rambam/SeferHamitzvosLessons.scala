package org.podval.calendar.rambam

import org.digitaljudaica.metadata.{Language, Metadata, Name, Names, WithNames}
import org.digitaljudaica.xml.{From, Parser, Xml}

object SeferHamitzvosLessons {

  final class Lesson(
    val number: Int,
    val parts: Seq[Part]
  )

  sealed trait Part extends WithNames

  final class NamedPart(override val names: Names) extends Part

  sealed abstract class Commandment(val number: Int) extends Part

  final class Positive(number: Int) extends Commandment(number) {
    override def names: Names = positive.withNumber(number)
  }

  private val positive: Names = new Names(Seq(
    Name("positive", Language.English.toSpec),
    Name("повелевающая", Language.Russian.toSpec),
    Name("עשה", Language.Hebrew.toSpec)
  ))

  final class Negative(number: Int) extends Commandment(number) {
    override def names: Names = negative.withNumber(number)
  }

  private val negative: Names = new Names(Seq(
    Name("negative", Language.English.toSpec),
    Name("запрещающая", Language.Russian.toSpec),
    Name("לא תעשה", Language.Hebrew.toSpec)
  ))

  val lessons: Seq[Lesson] = {
    val result: Seq[Lesson] = Metadata.load(
      from = From.resource(this),
      elementName = "lesson",
      parser = lessonParser
    )

    require(result.map(_.number) == (1 to RambamSchedule.numberOfLessons))

    result
  }

  private def lessonParser: Parser[Lesson] = for {
    number <- Xml.attribute.required.positiveInt("n")
    parts <- Xml.all[Part](partParser)
  } yield new Lesson(number, parts)

  private def partParser: Parser[Part] = for {
    name <- Xml.name
    result <- name match {
      case "positive" => Xml.attribute.required.positiveInt("n").map(new Positive(_))
      case "negative" => Xml.attribute.required.positiveInt("n").map(new Negative(_))
      case "named" => Names.parser.map(new NamedPart(_))
// TODO pre-check? Can't do it in the match, since check(): Unit case name => Parse.check(false, s"Unexpected element '$name'")
    }
  } yield result
}
