package org.opentorah.texts

import org.podval.store.{Selector, Selectors as SelectorCatalog}
import org.podval.xml.XmlParser

object Selectors extends SelectorCatalog:
  given SelectorCatalog = this
  override lazy val valuesSeq: Seq[Selector] = XmlParser.loadCatalog(this, "Selector", Selector.codec)

package tanach:
  export org.opentorah.texts.Selectors.given

package rambam:
  export org.opentorah.texts.Selectors.given
