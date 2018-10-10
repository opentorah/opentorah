package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.{Attributes, LanguageSpec}

final case class Verse(chapter: Int, verse: Int) extends Ordered[Verse] {
  require(chapter > 0)
  require(verse > 0)

  override def compare(that: Verse): Int = {
    val result = this.chapter - that.chapter
    if (result != 0) result else this.verse - that.verse
  }

  override def toString: String = toString(LanguageSpec.empty)

  def toString(spec: LanguageSpec): String = spec.toString(chapter) + ":" + spec.toString(verse)
}

object Verse {

  final case class Parsed(chapter: Option[Int], verse: Option[Int]) {

    def inheritFrom(ancestor: Parsed): Parsed = {
      require(this.chapter.isEmpty || ancestor.chapter.isEmpty)
      require(this.verse.isEmpty || ancestor.verse.isEmpty)

      Parsed(
        chapter = this.chapter.orElse(ancestor.chapter),
        verse = this.verse.orElse(ancestor.verse),
      )
    }

    def resolve: Verse = Verse(chapter.get, verse.get)
  }

  final def parseFrom(attributes: Attributes): Parsed = Parsed(
    chapter = attributes.getInt("fromChapter"),
    verse = attributes.getInt("fromVerse")
  )

  final def parseTo(attributes: Attributes): Parsed = Parsed(
    chapter = attributes.getInt("toChapter"),
    verse = attributes.getInt("toVerse")
  )
}
