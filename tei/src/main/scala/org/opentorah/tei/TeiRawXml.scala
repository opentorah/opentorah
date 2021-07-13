package org.opentorah.tei

import org.opentorah.xml.RawXml

class TeiRawXml(
  elementName: String,
  attributesAllowed: Boolean = false
) extends RawXml(
  elementName: String,
  namespace = Some(Tei.namespace),
  attributesAllowed = attributesAllowed
)
