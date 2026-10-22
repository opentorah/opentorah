package org.opentorah.texts

import org.podval.metadata.{Term, TermCatalog}
import org.podval.xml.XmlParser

object Terms:
  given catalog: TermCatalog = TermCatalog(XmlParser.loadCatalog(this, Term.codec))

package tanach:
  export org.opentorah.texts.Terms.given

package rambam:
  export org.opentorah.texts.Terms.given
