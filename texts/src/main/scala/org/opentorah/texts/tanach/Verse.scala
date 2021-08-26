package org.opentorah.texts.tanach

import org.opentorah.metadata.{Language, LanguageSpec}

final case class Verse(chapter: Int, verse: Int) extends Ordered[Verse] with Language.ToString {
  require(chapter > 0)
  require(verse > 0)

  override def compare(that: Verse): Int = {
    val result = this.chapter - that.chapter
    if (result != 0) result else this.verse - that.verse
  }

  override def toLanguageString(implicit spec: LanguageSpec): String =
    spec.toString(chapter) + ":" + spec.toString(verse)
}
