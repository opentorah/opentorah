package org.podval.docbook.gradle.xml

import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl

final case class AttributeInfo(
  localName: String,
  qName: String,
  value: String,
  `type`: String,
  uri: String
) {
  val namespace: Option[Namespace] = {
    val colon: Int = qName.indexOf(":")
    if (colon == -1) None else {
      val tail = qName.substring(colon + 1)
      require(tail == localName)
      Some(new Namespace(prefix = qName.substring(0, colon), uri = uri))
    }
  }

  def isXmlns: Boolean = namespace.map(_.prefix).contains(Namespace.Xmlns.prefix)

  def declaredNamespace: Option[Namespace] =
    if (!isXmlns) None else Some(new Namespace(prefix = localName, uri = value))

  override def toString: String =
    s"AttributeInfo(localName=$localName, qName=$qName, value=$value; type=${`type`}; uri=$uri; namespace=$namespace)"

  def addTo(attributes: AttributesImpl): Unit = attributes.addAttribute(uri, localName, qName, `type`, value)
}

object AttributeInfo {

  def apply(attributes: Attributes, index: Int): AttributeInfo = AttributeInfo(
    localName = attributes.getLocalName(index),
    qName = attributes.getQName(index),
    value = attributes.getValue(index),
    `type` = attributes.getType(index),
    uri = attributes.getURI(index)
  )

  def apply(attributes: Attributes): Seq[AttributeInfo] =
    for (index: Int <- 0 until attributes.getLength) yield AttributeInfo(attributes, index)
}
