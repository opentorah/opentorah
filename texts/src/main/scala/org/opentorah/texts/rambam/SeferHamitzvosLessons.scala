package org.opentorah.texts.rambam

import org.opentorah.metadata.{HasName, Language, Name, Named, Names}
import org.opentorah.store.Selector
import org.opentorah.xml.{Attribute, Element, Elements, From, Parsable, Parser, Unparser}

object SeferHamitzvosLessons:

  private val nAttribute: Attribute.Required[Int] = Attribute.PositiveIntAttribute("n").required

  final class Lesson(
    val number: Int,
    val parts: Seq[Part]
  )

  object Lesson extends Element[Lesson]("lesson"):
    override def contentParsable: Parsable[Lesson] = new Parsable[Lesson]:
      override def parser: Parser[Lesson] = for
        number: Int <- nAttribute()
        parts: Seq[Part] <- Part.seq()
      yield Lesson(
        number,
        parts
      )

      override def unparser: Unparser[Lesson] = ???

  sealed trait Part extends Named

  private object Part extends Elements.Union[Part]:
    override protected val elements: Seq[Element[? <: Part]] = Seq(Positive, Negative, NamedPart)

    override protected def elementByValue(value: Part): Element[? <: Part] = value match
      case _: Positive  => Positive
      case _: Negative  => Negative
      case _: NamedPart => NamedPart

  final case class NamedPart(override val names: Names) extends Part

  object NamedPart extends Element[NamedPart]("named"):
    override def contentParsable: Parsable[NamedPart] = new Parsable[NamedPart]:
      override def parser: Parser[NamedPart] = Names.withoutDefaultNameParsable().map(NamedPart.apply)
      override def unparser: Unparser[NamedPart] = Names.withoutDefaultNameParsable(_.names)

  private class Numbered(elementName: String) extends Element[Positive](elementName):
    def selector: Selector = Selector.getForName(elementName)

    override def contentParsable: Parsable[Positive] = new Parsable[Positive]:
      override def parser: Parser[Positive] = nAttribute().map(Positive.apply)
      override def unparser: Unparser[Positive] = nAttribute(_.number)

  sealed abstract class Commandment(val number: Int) extends Part:
    final override def names: Names = selector.andNumber(number).names
    def selector: Selector

  final case class Positive(override val number: Int) extends Commandment(number):
    override def selector: Selector = Positive.selector

  private object Positive extends Numbered("positive")

  final case class Negative(override val number: Int) extends Commandment(number):
    override def selector: Selector = Negative.selector

  private object Negative extends Numbered("negative")

  // unless this is lazy, ZIO deadlocks; see https://github.com/zio/zio/issues/1841
  lazy val lessons: Seq[Lesson] = Parser.unsafeRun(HasName.load(From.resource(this), Lesson))
