package org.opentorah.calendar.rambam

import org.opentorah.metadata.{Language, Metadata, Name, Names, WithNames}
import org.opentorah.xml.{Attribute, Element, From, Parsable, Parser, UnionParsable}

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

  private val partParsable: Parsable[Part] = new UnionParsable[Part](Seq(
    new Element("positive", parser = Attribute("n").positiveInt.required.map(new Positive(_))),
    new Element("negative", parser = Attribute("n").positiveInt.required.map(new Negative(_))),
    new Element("named", parser = Names.parser.map(new NamedPart(_)))
  ))

  private def lessonParser: Parser[Lesson] = for {
    number <- Attribute("n").positiveInt.required
    parts <- partParsable.allMustBe
  } yield new Lesson(number, parts)
}
