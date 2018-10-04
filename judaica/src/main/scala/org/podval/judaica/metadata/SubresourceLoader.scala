package org.podval.judaica.metadata

import scala.xml.Elem

trait SubresourceLoader extends HasMetadata {
  protected final def loadSubresource(element: Elem): PreparsedMetadata = {
    val (attributes, elements) = XML.open(element, elementName)
    attributes.get("resource").fold(pack(attributes, elements)) { subresourceName: String =>
      attributes.close()
      val subresource: Elem = loadResource(subresourceName)
      val (newAttributes, newElements) = XML.open(subresource, elementName)
      pack(newAttributes, newElements)
    }
  }

  private def pack(attributes: Attributes, elements: Seq[Elem]): PreparsedMetadata = {
    val (names: Names, tail: Seq[Elem]) = Names.parse(attributes, elements)
    PreparsedMetadata(attributes, names, tail)
  }

  protected def elementName: String
}
