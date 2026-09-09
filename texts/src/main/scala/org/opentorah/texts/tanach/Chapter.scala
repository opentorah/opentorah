package org.opentorah.texts.tanach

import org.podval.store.{By, NumberedStore, NumberedStores, Stores}

abstract class Chapter(override val number: Int, from: Int, to: Int) extends NumberedStore, Stores[?]:
  def length: Int = to - from + 1

  private lazy val verses: By[Verse] = By.numbered("verse", from, to): (number, parent) =>
    new Verse(number):
      override def oneOf: NumberedStores[Verse] = parent

  override def stores: Seq[By[?]] = Seq(verses)
