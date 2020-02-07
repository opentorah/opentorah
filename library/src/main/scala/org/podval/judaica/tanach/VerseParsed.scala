package org.podval.judaica.tanach

import cats.implicits._
import org.digitaljudaica.metadata.{Attributes, Xml}

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

  final def parseFrom(attributes: Attributes): VerseParsed = VerseParsed(
    chapter = attributes.getInt("fromChapter"),
    verse = attributes.getInt("fromVerse")
  )

  val fromParser: Xml.Parser[VerseParsed] = for {
    chapter <- Xml.optionalIntAttribute("fromChapter")
    verse <- Xml.optionalIntAttribute("fromVerse")
  } yield VerseParsed(chapter, verse)

  final def parseTo(attributes: Attributes): VerseParsed = VerseParsed(
    chapter = attributes.getInt("toChapter"),
    verse = attributes.getInt("toVerse")
  )

  val toParser: Xml.Parser[VerseParsed] = for {
    chapter <- Xml.optionalIntAttribute("toChapter")
    verse <- Xml.optionalIntAttribute("toVerse")
  } yield VerseParsed(chapter, verse)
}
