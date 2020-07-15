package org.opentorah.xml

import org.opentorah.util.Strings
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

    override def toString: String =
      s"AttributeInfo(localName=$localName, qName=$qName, value=$value; type=${`type`}; uri=$uri; namespace=$namespace)"

    val namespace: Option[Namespace] = {
      val (prefix: Option[String], tail: String) = Strings.splitRight(qName, ':')
      prefix.map { prefix =>
        require(tail == localName)
        new Namespace(prefix = prefix, uri = uri)
      }
    }

    def isXmlns: Boolean = namespace.exists(_.isXmlns)

    def declaredNamespace: Option[Namespace] =
      if (!isXmlns) None else Some(new Namespace(prefix = localName, uri = value))

    def addTo(attributes: AttributesImpl): Unit = attributes.addAttribute(uri, localName, qName, `type`, value)
  }

  // Note: XMLObj.setAttributes() sets namespace on an attribute only if it already saw
  // the declarations of that namespace, so I am making sure that they are there (and in the beginning);
  // even then, XMLObj.setAttributes() sets un-prefixed qualified name for namespaced attributes -
  // but somehow they are detected correctly in MathJax.typeset()...
  def sort(attributes: Attributes): AttributesImpl = {
    val infos: Seq[AttributeInfo] = for (index: Int <- 0 until attributes.getLength) yield AttributeInfo(
      localName = attributes.getLocalName(index),
      qName = attributes.getQName(index),
      value = attributes.getValue(index),
      `type` = attributes.getType(index),
      uri = attributes.getURI(index)
    )

    val nonXmlnsAttributes: Seq[AttributeInfo] = infos.filterNot(_.isXmlns)
    val usedNamespaces: Set[Namespace] = nonXmlnsAttributes.flatMap(_.namespace).toSet
    val declaredNamespaces: Set[Namespace] = infos.flatMap(_.declaredNamespace).toSet

    val result: AttributesImpl = new AttributesImpl

    for (namespace: Namespace <- usedNamespaces -- declaredNamespaces) namespace.declare(result)

    for (attribute: AttributeInfo <- nonXmlnsAttributes) attribute.addTo(result)

    result
  }
}
