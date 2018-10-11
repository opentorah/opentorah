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

  def inBook(book: Tanach.ChumashBook): BookSpan.ChumashSpan.BookSpan = BookSpan.ChumashSpan.BookSpan(book, this)

  def inBook(book: Tanach.ProphetsBook): BookSpan.ProphetSpan.BookSpan = BookSpan.ProphetSpan.BookSpan(book, this)
}

object Span {

  final case class Parsed(
    from: Verse.Parsed,
    to: Verse.Parsed
  ) {
    def inheritFrom(ancestor: Parsed): Parsed = Parsed(
      from = this.from.inheritFrom(ancestor.from),
      to = this.to.inheritFrom(ancestor.to)
    )

    def defaultFromChapter(fromChapter: Int): Parsed =
      Parsed(from = from.defaultChapter(fromChapter), to = to)

    def semiResolve: SemiResolved = {
      val fromResult = from.resolve

      require(to.verse.nonEmpty || to.chapter.isEmpty)

      val toResult = if (to.verse.isEmpty) None else Some(Verse(
        chapter = to.chapter.getOrElse(fromResult.chapter),
        verse = to.verse.get
      ))

      SemiResolved(fromResult, toResult)
    }

    def resolve: Span = {
      val fromResult = from.resolve

      val toResult = Verse(
        chapter = to.chapter.getOrElse(fromResult.chapter),
        verse = to.verse.getOrElse(fromResult.verse)
      )

      Span(fromResult, toResult)
    }
  }

  final def parse(attributes: Attributes): Parsed = Parsed(
    from = Verse.parseFrom(attributes),
    to = Verse.parseTo(attributes)
  )

  final case class SemiResolved(from: Verse, to: Option[Verse]) {
    def setTo(value: Verse): Span = {
      require(to.isEmpty || to.contains(value), "Wrong explicit 'to'")
      Span(from, value)
    }
  }

  def setImpliedTo(
    spans: Seq[SemiResolved],
    span: Span,
    chapters: Chapters
  ): Seq[Span] = {
    val tos: Seq[Verse] = spans.tail.map(_.from).map(chapters.prev(_).get) :+ span.to
    val result = spans.zip(tos).map { case (s, to) => s.setTo(to) }
    require(chapters.cover(result, span))

    result
  }

  final case class Numbered(override val n: Int, span: SemiResolved) extends WithNumber

  def addImplied1(
    spans: Seq[Numbered],
    span: Span,
    chapters: Chapters
  ): Seq[Numbered] = {
    val first = spans.head
    val implied1: Seq[Numbered] = if (first.n == 1) Seq.empty else Seq(Numbered(1, SemiResolved(
      span.from,
      Some(chapters.prev(first.span.from).get)
    )))

    implied1 ++ spans
  }
}
