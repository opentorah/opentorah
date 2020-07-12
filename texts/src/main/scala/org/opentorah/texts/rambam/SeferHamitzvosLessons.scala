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

  private val numberedParser: Parser[Int] = Attribute.positiveInt("n").required

  private val partParsable: Parsable[Part] = new UnionParsable[Part](Seq(
    new Element[Part]("positive") { override protected def parser: Parser[Positive] = numberedParser.map(Positive) },
    new Element[Part]("negative") { override protected def parser: Parser[Negative] = numberedParser.map(Negative) },
    new Element[Part]("named") { override protected def parser: Parser[NamedPart] = Names.parser.map(NamedPart) }
  ))

  private val lessonParser: Parser[Lesson] = for {
    number <- Attribute.positiveInt("n").required
    parts <- partParsable.all
  } yield Lesson(number, parts)

  // unless this is lazy, ZIO deadlocks; see https://github.com/zio/zio/issues/1841
  lazy val lessons: Seq[Lesson] = Metadata.load(
    from = From.resource(this),
    elementParsable = new Element[Lesson]("lesson") { override protected def parser: Parser[Lesson] = lessonParser }
  )
}
