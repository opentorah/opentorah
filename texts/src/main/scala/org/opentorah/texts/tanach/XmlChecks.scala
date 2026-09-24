package org.opentorah.texts.tanach

import org.podval.xml.{Xml, XmlError}

/** Throwing reads for Tanach fragments that are not codec records. */
private[tanach] object XmlChecks:
  def requireName(element: Xml.Element, name: String): Unit =
    if !element.isNamed(name) then throw XmlError(s"Expected '$name', found '${element.getName.qName}'")

  def requireAttr(element: Xml.Element, name: String): String =
    element.get(name).map(_.trim).filter(_.nonEmpty).getOrElse:
      throw XmlError(s"Missing attribute '$name'")

  def requireNoOther(element: Xml.Element, allowed: Set[String]): Unit =
    val extra: Seq[String] = element.childElements.map(_.getName.localName).filterNot(allowed.contains)
    if extra.nonEmpty then throw XmlError(s"Unparsed elements: $extra")

  def positiveInt(element: Xml.Element, name: String): Int =
    val raw: String = requireAttr(element, name)
    val n: Int = raw.toIntOption.getOrElse(throw XmlError(s"Invalid integer for $name: $raw"))
    if n <= 0 then throw XmlError(s"Non-positive integer: $n")
    n
