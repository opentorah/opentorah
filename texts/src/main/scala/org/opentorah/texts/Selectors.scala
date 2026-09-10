package org.opentorah.texts

import org.podval.store.Selectors as SelectorCatalog

object Selectors extends SelectorCatalog:
  given SelectorCatalog = this

package tanach:
  export org.opentorah.texts.Selectors.given

package rambam:
  export org.opentorah.texts.Selectors.given
