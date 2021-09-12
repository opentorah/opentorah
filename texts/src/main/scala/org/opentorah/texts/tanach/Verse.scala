package org.opentorah.texts.tanach

import org.opentorah.metadata.{Language, LanguageSpec}

// TODO rename to ChapterAndVerse, and rename VerseStore to Verse...
final case class Verse(chapter: Int, verse: Int) extends Ordered[Verse], Language.ToString:
  require(chapter > 0)
  require(verse > 0)

  override def compare(that: Verse): Int =
    val result = this.chapter - that.chapter
    if result != 0 then result else this.verse - that.verse

  override def toLanguageString(using spec: LanguageSpec): String =
    spec.toString(chapter) + ":" + spec.toString(verse)
