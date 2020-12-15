package org.opentorah.texts.rambam

import org.opentorah.metadata.{Language, Metadata, Name, Names, WithNames}
import org.opentorah.xml.{Antiparser, Attribute, Element, From, Parser, ToXml, Xml}

object SeferHamitzvosLessons {

  private val nAttribute: Attribute.PositiveIntAttribute = new Attribute.PositiveIntAttribute("n")

  final case class Lesson(
    number: Int,
    parts: Seq[Part]
  )

  object Lesson extends Element[Lesson]("lesson") {
    override def parser: Parser[Lesson] = for {
      number <- nAttribute.required
      parts <- Part.all
    } yield Lesson(number, parts)

    override def antiparser: Antiparser[Lesson] = ???
  }

  sealed trait Part extends WithNames

  // TODO extend Union from ToXml:
  private object Part extends Element.Union[Part] with ToXml[Part] {
    override protected val elements: Seq[Element[_ <: Part]] = Seq(Positive, Negative, NamedPart)

    override def toXmlElement(value: Part): Xml.Element = value match {
      case value: Positive  => Positive .toXmlElement(value)
      case value: Negative  => Negative .toXmlElement(value)
      case value: NamedPart => NamedPart.toXmlElement(value)
    }
  }

  final case class NamedPart(override val names: Names) extends Part

  object NamedPart extends Element[NamedPart]("named") {
    override def parser: Parser[NamedPart] = Names.withoutDefaultNameParser.map(apply)

    override def antiparser: Antiparser[NamedPart] = Names.antiparser.compose(_.names)
  }

  sealed abstract class Commandment(val number: Int) extends Part

  final case class Positive(override val number: Int) extends Commandment(number) {
    override def names: Names = Positive.names.withNumber(number)
  }

  object Positive extends Element[Positive]("positive") {
    private val names: Names = new Names(Seq(
      Name("positive", Language.English.toSpec),
      Name("повелевающая", Language.Russian.toSpec),
      Name("עשה", Language.Hebrew.toSpec)
    ))

    override def parser: Parser[Positive] = nAttribute.required.map(apply)

    override def antiparser: Antiparser[Positive] = nAttribute.toXml.compose(_.number)
  }

  final case class Negative(override val number: Int) extends Commandment(number) {
    override def names: Names = Negative.names.withNumber(number)
  }

  object Negative extends Element[Negative]("negative") {
    private val names: Names = new Names(Seq(
      Name("negative", Language.English.toSpec),
      Name("запрещающая", Language.Russian.toSpec),
      Name("לא תעשה", Language.Hebrew.toSpec)
    ))

    override def parser: Parser[Negative] = nAttribute.required.map(apply)

    override def antiparser: Antiparser[Negative] = nAttribute.toXml.compose(_.number)
  }

  // unless this is lazy, ZIO deadlocks; see https://github.com/zio/zio/issues/1841
  lazy val lessons: Seq[Lesson] = Parser.parseDo(Metadata.load(
    from = From.resource(this),
    fromXml = Lesson
  ))
}
