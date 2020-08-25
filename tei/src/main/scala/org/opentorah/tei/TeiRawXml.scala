package org.opentorah.tei

import org.opentorah.xml.RawXml

class TeiRawXml(elementName: String) extends RawXml(elementName: String, Some(Tei.namespace))