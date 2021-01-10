package org.opentorah.tei

import org.opentorah.xml.RawXml

class TeiRawXml(elementName: String) extends RawXml(elementName: String, namespace = Some(Tei.namespace))