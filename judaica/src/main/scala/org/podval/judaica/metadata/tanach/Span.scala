package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, LanguageSpec}

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

  final case class Raw(
    fromChapter: Option[Int],
    fromVerse: Option[Int],
    toChapter: Option[Int],
    toVerse: Option[Int]
  ) {
    def inheritFrom(ancestor: Raw): Raw = {
      require(this.fromChapter.isEmpty || ancestor.fromChapter.isEmpty)
      require(this.fromVerse.isEmpty || ancestor.fromVerse.isEmpty)
      require(this.toChapter.isEmpty || ancestor.toChapter.isEmpty)
      require(this.toVerse.isEmpty || ancestor.toVerse.isEmpty)

      Raw(
        fromChapter = this.fromChapter.orElse(ancestor.fromChapter),
        fromVerse = this.fromVerse.orElse(ancestor.fromVerse),
        toChapter = this.toChapter.orElse(ancestor.toChapter),
        toVerse = this.toVerse.orElse(ancestor.toVerse)
      )
    }

    def parse: Parsed = {
      val from = resolveFrom

      require(toVerse.nonEmpty || toChapter.isEmpty)

      val to = if (toVerse.isEmpty) None else Some(Verse(
        chapter = toChapter.getOrElse(from.chapter),
        verse = toVerse.get
      ))

      new Parsed(from, to)
    }

    def resolve: Span = {
      val from = resolveFrom

      val to = Verse(
        chapter = toChapter.getOrElse(fromChapter.get),
        verse = toVerse.getOrElse(fromVerse.get)
      )

      Span(from, to)
    }

    private def resolveFrom: Verse = {
      require(fromChapter.isDefined)
      require(fromVerse.isDefined)

      Verse(
        fromChapter.get,
        fromVerse.get
      )
    }
  }

  final def parseRaw(attributes: Attributes): Raw = Raw(
    fromChapter = attributes.getInt("fromChapter"),
    fromVerse = attributes.getInt("fromVerse"),
    toChapter = attributes.getInt("toChapter"),
    toVerse = attributes.getInt("toVerse")
  )

  final class Parsed(val from: Verse, val to: Option[Verse]) {
    def setTo(value: Verse): Span = {
      require(to.isEmpty || to.contains(value), "Wrong explicit 'to'")
      Span(from, value)
    }
  }

  def parse(attributes: Attributes): Parsed = parseRaw(attributes).parse

  def setImpliedTo(
    spans: Seq[Parsed],
    span: Span,
    chapters: Chapters
  ): Seq[Span] = {
    val tos: Seq[Verse] = spans.tail.map(_.from).map(chapters.prev(_).get) :+ span.to
    val result = spans.zip(tos).map { case (s, to) => s.setTo(to) }
    require(chapters.cover(result, span))

    result
  }
}
