package org.opentorah.texts.rambam

import org.opentorah.metadata.{Language, Metadata, Name, Names, WithNames}
import org.opentorah.xml.{Unparser, Attribute, Element, Elements, Parsable, Parser}

object SeferHamitzvosLessons {

  private val nAttribute: Attribute.Required[Int] = new Attribute.PositiveIntAttribute("n").required

  final case class Lesson(
    number: Int,
    parts: Seq[Part]
  )

  object Lesson extends Element[Lesson]("lesson") {
    override def contentParsable: Parsable[Lesson] = new Parsable[Lesson] {
      override def parser: Parser[Lesson] = for {
        number <- nAttribute()
        parts <- Part.seq()
      } yield Lesson(number, parts)

      override def unparser: Unparser[Lesson] = ???
    }
  }

  sealed trait Part extends WithNames

  private object Part extends Elements.Union[Part] {
    override protected val elements: Seq[Element[_ <: Part]] = Seq(Positive, Negative, NamedPart)

    override protected def elementByValue(value: Part): Element[_ <: Part] = value match {
      case _: Positive  => Positive
      case _: Negative  => Negative
      case _: NamedPart => NamedPart
    }
  }

  final case class NamedPart(override val names: Names) extends Part

  object NamedPart extends Element[NamedPart]("named") {
    override def contentParsable: Parsable[NamedPart] = new Parsable[NamedPart] {
      override def parser: Parser[NamedPart] = Names.withoutDefaultNameParsable().map(NamedPart.apply)
      override def unparser: Unparser[NamedPart] = Names.withoutDefaultNameParsable(_.names)
    }
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

    override def contentParsable: Parsable[Positive] = new Parsable[Positive] {
      override def parser: Parser[Positive] = nAttribute().map(Positive.apply)
      override def unparser: Unparser[Positive] = nAttribute(_.number)
    }
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

    override def contentParsable: Parsable[Negative] = new Parsable[Negative] {
      override def parser: Parser[Negative] = nAttribute().map(Negative.apply)
      override def unparser: Unparser[Negative] = nAttribute(_.number)
    }
  }

  // unless this is lazy, ZIO deadlocks; see https://github.com/zio/zio/issues/1841
  lazy val lessons: Seq[Lesson] = Parser.unsafeRun(Metadata.loadResource(this, Lesson))
}
