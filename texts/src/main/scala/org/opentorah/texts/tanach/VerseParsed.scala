package org.opentorah.texts.tanach

import org.podval.xml.Xml

final class VerseParsed(val chapter: Option[Int], val verse: Option[Int]):
  def inheritFrom(ancestor: VerseParsed): VerseParsed =
    VerseParsed(
      chapter = this.chapter.orElse(ancestor.chapter),
      verse = this.verse.orElse(ancestor.verse),
    )

  def defaultChapter(defaultChapter: Int): VerseParsed =
    if chapter.isDefined then this
    else VerseParsed(chapter = Some(defaultChapter), verse = verse)

  def resolve: ChapterAndVerse = ChapterAndVerse(chapter.get, verse.getOrElse(1))

object VerseParsed:

  def parse(value: String): VerseParsed =
    val trimmed: String = value.trim
    trimmed.split(':').toSeq.map(_.trim) match
      case Seq(chapter) =>
        VerseParsed(Some(positive(chapter, trimmed)), None)
      case Seq(chapter, verse) =>
        VerseParsed(Some(positive(chapter, trimmed)), Some(positive(verse, trimmed)))
      case _ =>
        throw IllegalArgumentException(s"Citation must be chapter or chapter:verse, got '$value'")

  def parseOpt(value: Option[String]): VerseParsed =
    value.map(_.trim).filter(_.nonEmpty).fold(VerseParsed(None, None))(parse)

  def decodeFrom(element: Xml.Element): VerseParsed = parseOpt(element.get("from"))

  def decodeTo(element: Xml.Element): VerseParsed = parseOpt(element.get("to"))

  private def positive(raw: String, whole: String): Int =
    val n: Int = raw.toIntOption.getOrElse(throw IllegalArgumentException(
      s"Citation must be chapter or chapter:verse, got '$whole'"))
    require(n > 0, s"Citation '$whole' must be positive")
    n
