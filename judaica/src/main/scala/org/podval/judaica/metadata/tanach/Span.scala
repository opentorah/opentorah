package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, LanguageSpec, XML}

import scala.xml.Elem

final case class Span(from: Verse, to: Verse) {
  require(from <= to, s"Empty span: $from..$to")

  def contains(verse: Verse): Boolean = (from <= verse) && (verse <= to)

  // Assuming that Chapters.consecutive(this, that) returned 'true'.
  // def merge(that: Span): Span = Span(this.from, that.to)

  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String =
    if (from.chapter != to.chapter) from.toString(spec) + "-" + to.toString(spec)
    else spec.toString(from.chapter) + ":" +
      (if (from.verse == to.verse) spec.toString(from.verse)
      else spec.toString(from.verse) + "-" + spec.toString(to.verse))
}

object Span {
  final class Parsed(val from: Verse, val to: Option[Verse]) {
    def setTo(value: Verse): Span = {
      require(to.isEmpty || to.contains(value), "Wrong explicit 'to'")
      Span(from, value)
    }
  }

  def parse(element: Elem, name: String): Parsed = {
    val attributes = XML.openEmpty(element, name)
    val result = parse(attributes)
    attributes.close()
    result
  }

  def parse(attributes: Attributes): Parsed = {
    val from = parseFrom(attributes)
    val toChapter = attributes.getInt("toChapter")
    val toVerse = attributes.getInt("toVerse")
    val to = if (toVerse.isEmpty) {
      require(toChapter.isEmpty)
      None
    } else {
      Some(Verse(toChapter.getOrElse(from.chapter), toVerse.get))
    }
    new Parsed(from, to)
  }

  private def parseFrom(attributes: Attributes): Verse = Verse(
    attributes.doGetInt("fromChapter"),
    attributes.doGetInt("fromVerse")
  )

  def setImpliedTo(
    spans: Seq[Parsed],
    span: Span,
    chapters: Chapters
  ): Seq[Span] = {
    val tos: Seq[Verse] = spans.tail.map(_.from).map(chapters.prev(_).get) :+ span.to
    val result = spans.zip(tos).map { case (span, to) => span.setTo(to) }
    require(chapters.cover(result, span))

    result
  }
}
