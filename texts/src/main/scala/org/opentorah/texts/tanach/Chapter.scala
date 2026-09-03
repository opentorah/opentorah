package org.opentorah.texts.tanach

import org.podval.store.{By, NumberedStore, NumberedStores, Pure}

abstract class Chapter(override val number: Int, from: Int, to: Int) extends NumberedStore, Pure[?]:
  def length: Int = to - from + 1
  override def storesPure: Seq[By[?]] = Seq(Chapter.ByVerse(from, to))

object Chapter:

  final class ByVerse(override val minNumber: Int, override val maxNumber: Int) extends
    By.WithSelector[Verse]("verse"),
    NumberedStores[Verse]:
    override protected def createNumberedStore(number: Int): Verse = new Verse(number):
      override def oneOf: NumberedStores[Verse] = ByVerse.this
