package org.opentorah.texts

import org.podval.store.{Selector, Selectors as SelectorCatalog}
import org.podval.xml.XmlParser

object Selectors extends SelectorCatalog:
  override lazy val valuesSeq: Seq[Selector] = XmlParser.loadCatalog(this, "Selector", Selector.codec)

given SelectorCatalog = Selectors
