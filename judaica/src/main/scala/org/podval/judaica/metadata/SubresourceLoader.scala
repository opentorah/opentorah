package org.podval.judaica.metadata

import scala.xml.Elem

trait SubresourceLoader extends HasMetadata {
  protected final def loadSubresource(element: Elem): PreparsedMetadata = {
    val (attributes: Attributes, namesOption: Option[Names], elements: Seq[Elem]) = Names.parse(element, elementName)
    val names: Names = namesOption.get

    if (elements.nonEmpty) PreparsedMetadata(attributes, names, elements) else {
      val subresources: Seq[Elem] = names.names.flatMap { name => loadResource(name.name) }
      require(subresources.size <= 1, s"More than one subresource: $subresources")
      if (subresources.isEmpty) PreparsedMetadata(attributes, names, elements) else {
        attributes.close()
        val (newAttributes: Attributes, newNames: Option[Names], newElements: Seq[Elem]) =
          Names.parse(subresources.head, elementName)
        PreparsedMetadata(
          newAttributes,
          newNames.fold(names)(Names.merge(names, _)),
          newElements
        )
      }
    }
  }

  protected def elementName: String
}
