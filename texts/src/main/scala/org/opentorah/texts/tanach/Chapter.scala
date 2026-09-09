package org.opentorah.texts.tanach

import org.podval.store.{By, NumberedStore, NumberedStores, Stores}

final class Chapter(
  override val number: Int,
  from: Int,
  to: Int,
  override val oneOf: NumberedStores[Chapter]
) extends NumberedStore, Stores[?]:
  def length: Int = to - from + 1

  private lazy val verses: By.Numbered[Verse] = By.Numbered("verse", from, to)(Verse(_, _))

  override def stores: Seq[By[?]] = Seq(verses)
