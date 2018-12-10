package org.podval.judaica.tanach

import org.podval.judaica.metadata.Attributes

final case class VerseParsed(chapter: Option[Int], verse: Option[Int]) {

  def inheritFrom(ancestor: VerseParsed): VerseParsed = {
    require(this.chapter.isEmpty || ancestor.chapter.isEmpty)
    require(this.verse.isEmpty || ancestor.verse.isEmpty)

    VerseParsed(
      chapter = this.chapter.orElse(ancestor.chapter),
      verse = this.verse.orElse(ancestor.verse),
    )
  }

  def defaultChapter(defaultChapter: Int): VerseParsed =
    if (chapter.isDefined) this
    else VerseParsed(chapter = Some(defaultChapter), verse = verse)

  def resolve: Verse = Verse(chapter.get, verse.get)
}

object VerseParsed {

  final def parseFrom(attributes: Attributes): VerseParsed = VerseParsed(
    chapter = attributes.getInt("fromChapter"),
    verse = attributes.getInt("fromVerse")
  )

  final def parseTo(attributes: Attributes): VerseParsed = VerseParsed(
    chapter = attributes.getInt("toChapter"),
    verse = attributes.getInt("toVerse")
  )
}
