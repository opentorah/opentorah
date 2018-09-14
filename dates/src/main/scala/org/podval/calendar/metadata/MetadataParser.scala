package org.podval.calendar.metadata

import java.io.FileNotFoundException
import java.net.URL

import org.podval.calendar.metadata.XML.open

import scala.xml.{Elem, Utility}

object MetadataParser {

  def getUrl(obj: AnyRef, name: String): URL =  {
    val result = obj.getClass.getResource(name + ".xml")
    require(result != null, s"No such resource: $obj:$name")
    result
  }

  final case class MetadataPreparsed(
    attributes: Attributes, // actual parser needs to call close()!
    names: Names,
    elements: Seq[Elem]
  ) extends HasNames

  def loadMetadata(obj: AnyRef, resourceName: String, name: String): Seq[MetadataPreparsed] =
    loadMetadata(getUrl(obj, resourceName), resourceName, name)

  def loadMetadata(url: URL, resourceName: String, name: String): Seq[MetadataPreparsed] = {
    val elements: Seq[Elem] = loadMetadataElements(url, "metadata", resourceName)
    preparseMetadata(url, elements, name)
  }

  def preparseMetadata(url: URL, elements: Seq[Elem], name: String): Seq[MetadataPreparsed] = {
    val result = elements.map(element => loadSubresource(url, element, name))
    Names.checkDisjoint(result.map(_.names))
    result
  }

  def bind[K <: Named, M <: HasNames](keys: Seq[K], metadatas: Seq[M]): Seq[(K, M)] = {
    require(keys.length == metadatas.length)
    keys.zip(metadatas).map { case (key, metadata) =>
      require(metadata.names.has(key.name))
      key -> metadata
    }
  }

  private def loadSubresource(url: URL, element: Elem, name: String ): MetadataPreparsed = {
    val (attributes: Attributes, namesOption: Option[Names], elements: Seq[Elem]) =
      NamesParser.parseNames(element, name)
    val names: Names = namesOption.get

    if (elements.nonEmpty) MetadataPreparsed(attributes, names, elements) else {
      val subresources: Seq[Elem] = names.names.flatMap(name => loadResource(siblingUrl(url, name.name)))
      require(subresources.size <= 1, "More than one subresource.")
      if (subresources.isEmpty) MetadataPreparsed(attributes, names, elements) else {
        attributes.close()
        val (newAttributes: Attributes, newNames: Option[Names], newElements: Seq[Elem]) =
          NamesParser.parseNames(subresources.head, name)
        MetadataPreparsed(
          newAttributes,
          newNames.fold(names)(Names.merge(names, _)),
          newElements
        )
      }
    }
  }

  private def loadMetadataElements(url: URL, rootElementName: String, what: String): Seq[Elem] = {
    val element = loadResource(url)
    require(element.isDefined, s"No resource: $url")
    val (attributes, elements) = open(element.get, rootElementName)
    val typeOption = attributes.get("type")
    attributes.close()
    require(typeOption.nonEmpty, "Attribute 'type' is missing.")
    require(typeOption.contains(what), s"Wrong metadata type: ${typeOption.get} instead of $what")
    elements
  }

  private def loadResource(url: URL): Option[Elem] = {
    try {
      val result = xml.XML.load(url.openStream())
      Some(Utility.trimProper(result).asInstanceOf[Elem])
    } catch {
      case _: FileNotFoundException => None
    }
  }

  def loadNames[K <: WithNames[K]](obj: AnyRef, name: String, keys: Seq[K]): Map[K, Names] = {
    val url = getUrl(obj, name)
    val elements = loadMetadataElements(url, "names", name)
    val metadatas: Seq[Names] = elements.map(element => NamesParser.parseNamesElement(element, None))
    Names.checkDisjoint(metadatas)

    // TODO relax the "same order" requirement.
    // TODO merge into bind()?
    require(keys.length == metadatas.length, s"Different lengths: $keys and $metadatas")
    val result = keys.zip(metadatas).map { case (key, metadata) =>
      require(metadata.has(key.name))
      key -> metadata
    }

    result.toMap
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
