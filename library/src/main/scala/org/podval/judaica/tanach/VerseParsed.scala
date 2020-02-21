package org.podval.judaica.tanach

import org.digitaljudaica.xml.{Parser, Xml}

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

  def resolve: Verse = Verse(chapter.get, verse.getOrElse(1))
}

object VerseParsed {

  val fromParser: Parser[VerseParsed] = for {
    chapter <- Xml.attribute.optional.positiveInt("fromChapter")
    verse <- Xml.attribute.optional.positiveInt("fromVerse")
  } yield VerseParsed(chapter, verse)

  val toParser: Parser[VerseParsed] = for {
    chapter <- Xml.attribute.optional.positiveInt("toChapter")
    verse <- Xml.attribute.optional.positiveInt("toVerse")
  } yield VerseParsed(chapter, verse)
}
