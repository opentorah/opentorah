package org.opentorah.texts.tanach

import org.opentorah.metadata.Language

final class ChapterAndVerse(val chapter: Int, val verse: Int) extends Ordered[ChapterAndVerse], Language.ToString derives CanEqual:
  require(chapter > 0)
  require(verse > 0)

  override def compare(that: ChapterAndVerse): Int =
    val result = this.chapter - that.chapter
    if result != 0 then result else this.verse - that.verse

  override def equals(other: Any): Boolean = compare(other.asInstanceOf[ChapterAndVerse]) == 0

  override def toLanguageString(using spec: Language.Spec): String =
    spec.toString(chapter) + ":" + spec.toString(verse)
