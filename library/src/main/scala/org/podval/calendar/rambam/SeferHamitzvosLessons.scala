package org.podval.calendar.rambam

import org.digitaljudaica.metadata.{Attributes, Language, Metadata, Name, Names, WithNames, Xml}

import scala.xml.Elem

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
    val result: Seq[Lesson] = Metadata
      .loadMetadataElements(this, None, "metadata", "lesson")
      .map(parseLesson)

    require(result.map(_.number) == (1 to RambamSchedule.numberOfLessons))

    result
  }

  private def parseLesson(element: Elem): Lesson = {
    val (attributes: Attributes, elements: Seq[Elem]) = Xml.open(element, "lesson")
    val number: Int = attributes.doGetInt("n")
    val parts: Seq[Part] = elements.map(parsePart)
    new Lesson(number, parts)
  }

  private def parsePart(element: Elem): Part = element.label match {
    case "positive" => new Positive(getNumber(element, "positive"))
    case "negative" => new Negative(getNumber(element, "negative"))

    case "named" =>
      val (attributes: Attributes, elements: Seq[Elem]) = Xml.open(element, "named")
      attributes.close()
      val (names: Names, tail: Seq[Elem]) = Names.parse(elements)
      Xml.checkNoMoreElements(tail)
      new NamedPart(names)
  }

  private def getNumber(element: Elem, name: String): Int = {
    val attributes: Attributes = Xml.openEmpty(element, name)
    val result = attributes.doGetInt("n")
    attributes.close()
    result
  }
}
