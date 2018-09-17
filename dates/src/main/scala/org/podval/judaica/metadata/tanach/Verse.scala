package org.podval.judaica.metadata.tanach

import org.podval.judaica.metadata.LanguageSpec

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
