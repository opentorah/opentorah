package org.opentorah.texts.rambam

import org.opentorah.metadata.{Language, Metadata, Name, Names, WithNames}
import org.opentorah.xml.{Attribute, Element, From, Parsable, Parser, UnionParsable}

object SeferHamitzvosLessons {

  final case class Lesson(
    number: Int,
    parts: Seq[Part]
  )

  sealed trait Part extends WithNames

  final case class NamedPart(override val names: Names) extends Part

  sealed abstract class Commandment(val number: Int) extends Part

  private val positive: Names = new Names(Seq(
    Name("positive", Language.English.toSpec),
    Name("повелевающая", Language.Russian.toSpec),
    Name("עשה", Language.Hebrew.toSpec)
  ))

  final case class Positive(override val number: Int) extends Commandment(number) {
    override def names: Names = positive.withNumber(number)
  }

  private val negative: Names = new Names(Seq(
    Name("negative", Language.English.toSpec),
    Name("запрещающая", Language.Russian.toSpec),
    Name("לא תעשה", Language.Hebrew.toSpec)
  ))

  final case class Negative(override val number: Int) extends Commandment(number) {
    override def names: Names = negative.withNumber(number)
  }

  private val numberedParser: Parser[Int] = Attribute("n").positiveInt.required

  private val partParsable: Parsable[Part] = new UnionParsable[Part](Seq(
    new Element("positive", parser = numberedParser.map(Positive)),
    new Element("negative", parser = numberedParser.map(Negative)),
    new Element("named", parser = Names.parser.map(NamedPart))
  ))

  private val lessonParser: Parser[Lesson] = for {
    number <- Attribute("n").positiveInt.required
    parts <- partParsable.all
  } yield Lesson(number, parts)

  // unless this is lazy, ZIO deadlocks; see https://github.com/zio/zio/issues/1841
  lazy val lessons: Seq[Lesson] = Metadata.load(
    from = From.resource(this),
    elementParsable = new Element[Lesson](elementName = "lesson", parser = lessonParser)
  )
}
