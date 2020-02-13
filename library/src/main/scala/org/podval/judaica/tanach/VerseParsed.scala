package org.podval.judaica.tanach

import cats.implicits._
import org.digitaljudaica.xml.{Attribute, Parser}

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
    chapter <- Attribute.optional.int("fromChapter")
    verse <- Attribute.optional.int("fromVerse")
  } yield VerseParsed(chapter, verse)

  val toParser: Parser[VerseParsed] = for {
    chapter <- Attribute.optional.int("toChapter")
    verse <- Attribute.optional.int("toVerse")
  } yield VerseParsed(chapter, verse)
}
