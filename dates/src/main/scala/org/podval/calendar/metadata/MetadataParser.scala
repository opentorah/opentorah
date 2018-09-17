package org.podval.calendar.metadata

import java.io.FileNotFoundException
import java.net.URL

import scala.xml.{Elem, Utility}

object MetadataParser {

  final case class MetadataPreparsed(
    attributes: Attributes, // actual parser needs to call close()!
    names: Names,
    elements: Seq[Elem]
  ) extends Named.HasNames

  def loadSubresource(url: URL, element: Elem, elementName: String ): MetadataPreparsed = {
    val (attributes: Attributes, namesOption: Option[Names], elements: Seq[Elem]) = Names.parse(element, elementName)
    val names: Names = namesOption.get

    if (elements.nonEmpty) MetadataPreparsed(attributes, names, elements) else {
      val subresources: Seq[Elem] = names.names.flatMap(name => loadResource(siblingUrl(url, name.name)))
      require(subresources.size <= 1, "More than one subresource.")
      if (subresources.isEmpty) MetadataPreparsed(attributes, names, elements) else {
        attributes.close()
        val (newAttributes: Attributes, newNames: Option[Names], newElements: Seq[Elem]) =
          Names.parse(subresources.head, elementName)
        MetadataPreparsed(
          newAttributes,
          newNames.fold(names)(Names.merge(names, _)),
          newElements
        )
      }
    }
  }

  def loadResource(url: URL): Option[Elem] = {
    try {
      val result = xml.XML.load(url.openStream())
      Some(Utility.trimProper(result).asInstanceOf[Elem])
    } catch {
      case _: FileNotFoundException => None
    }
  }

  private def siblingUrl(url: URL, name: String): URL = {
    val path = url.getFile
    val indexOfLastSlash: Int = path.lastIndexOf('/')
    require(indexOfLastSlash != -1)
    val parentPath = path.substring(0, indexOfLastSlash)
    val newPath = parentPath + "/" + name + ".xml"

    new URL(url.getProtocol, url.getHost, url.getPort, newPath, null)
  }
}
