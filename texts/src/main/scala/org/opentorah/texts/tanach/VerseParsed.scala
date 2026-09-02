package org.opentorah.texts.tanach

import org.podval.xml.XmlAst

final class VerseParsed(val chapter: Option[Int], val verse: Option[Int]):
  def inheritFrom(ancestor: VerseParsed): VerseParsed =
    require(this.chapter.isEmpty || ancestor.chapter.isEmpty)
    require(this.verse.isEmpty || ancestor.verse.isEmpty)

    VerseParsed(
      chapter = this.chapter.orElse(ancestor.chapter),
      verse = this.verse.orElse(ancestor.verse),
    )

  def defaultChapter(defaultChapter: Int): VerseParsed =
    if chapter.isDefined then this
    else VerseParsed(chapter = Some(defaultChapter), verse = verse)

  def resolve: ChapterAndVerse = ChapterAndVerse(chapter.get, verse.getOrElse(1))

object VerseParsed:

  def decodeFrom[E: XmlAst](element: E): VerseParsed = decode(element, "from")

  def decodeTo[E: XmlAst](element: E): VerseParsed = decode(element, "to")

  private def decode[E: XmlAst](element: E, prefix: String): VerseParsed = VerseParsed(
    chapter = XmlDecode.positiveIntOpt(element, prefix + "Chapter"),
    verse = XmlDecode.positiveIntOpt(element, prefix + "Verse")
  )
