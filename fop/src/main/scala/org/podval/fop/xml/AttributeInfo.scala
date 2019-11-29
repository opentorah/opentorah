package org.podval.fop.xml

import org.xml.sax.Attributes
import org.xml.sax.helpers.AttributesImpl

object AttributeInfo {

  private final case class AttributeInfo(
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

  private def apply(attributes: Attributes, index: Int): AttributeInfo = AttributeInfo(
    localName = attributes.getLocalName(index),
    qName = attributes.getQName(index),
    value = attributes.getValue(index),
    `type` = attributes.getType(index),
    uri = attributes.getURI(index)
  )

  // Note: XMLObj.setAttributes() sets namespace on an attribute only if it already saw
  // the declarations of that namespace, so I am making sure that they are there (and in the beginning);
  // even then, XMLObj.setAttributes() sets un-prefixed qualified name for namespaced attributes -
  // but somehow they are detected correctly in MathJax.typeset()...
  def sort(attlist: Attributes): AttributesImpl = {
    val attributes: Seq[AttributeInfo] =
      for (index: Int <- 0 until attlist.getLength) yield apply(attlist, index)

    val nonXmlnsAttributes: Seq[AttributeInfo] = attributes.filterNot(_.isXmlns)
    val usedNamespaces: Set[Namespace] = nonXmlnsAttributes.flatMap(_.namespace).toSet
    val declaredNamespaces: Set[Namespace] = attributes.flatMap(_.declaredNamespace).toSet

    val result: AttributesImpl = new AttributesImpl

    for (namespace: Namespace <- usedNamespaces -- declaredNamespaces) namespace.declare(result)

    for (attribute: AttributeInfo <- nonXmlnsAttributes) attribute.addTo(result)

    result
  }
}
