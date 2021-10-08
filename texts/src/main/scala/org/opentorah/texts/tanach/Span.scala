package org.opentorah.texts.tanach

import org.opentorah.metadata.Language

final class Span(val from: ChapterAndVerse, val to: ChapterAndVerse) extends Language.ToString derives CanEqual:
  require(from <= to, s"Empty span: $from..$to")

  override def equals(other: Any): Boolean =
    val that: Span = other.asInstanceOf[Span]
    (this.from == that.from) && (this.to == that.to)

  def contains(chapterAndVerse: ChapterAndVerse): Boolean = (from <= chapterAndVerse) && (chapterAndVerse <= to)

  override def toLanguageString(using spec: Language.Spec): String =
    if from.chapter != to.chapter then from.toLanguageString + "-" + to.toLanguageString
    else spec.toString(from.chapter) + ":" +
      (if from.verse == to.verse then spec.toString(from.verse)
      else spec.toString(from.verse) + "-" + spec.toString(to.verse))
