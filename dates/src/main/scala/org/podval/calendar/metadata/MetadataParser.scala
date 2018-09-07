package org.podval.calendar.metadata

import java.io.FileNotFoundException
import java.net.URL

import org.podval.calendar.metadata.XML.open

import scala.xml.{Elem, Utility}

// TODO sibling subresource.
// TODO pre-parse,  bind and load subresources in a general way; return Map[WithMetadata[M], M].
object MetadataParser {
  final case class MetadataPreparsed(
    attributes: Attributes, // actual parser needs to call close()!
    names: Names,
    elements: Seq[Elem]
  ) extends Metadata

  final class NamesOnlyMetadata(val names: Names) extends Metadata

  def getURL(name: String): URL = getClass.getClassLoader.getResource(name)

  private def childUrl(parent: URL, name: String): URL = changePath(parent, parent.getFile + "/" + name)

  def childFileUrl(parent: URL, name: String): URL = childUrl(parent, name + ".xml")

  private def parentUrl(url: URL): URL = {
    val path = url.getFile
    val indexOfLastSlash: Int = path.lastIndexOf('/')
    require(indexOfLastSlash != -1)

    changePath(url, path.substring(0, indexOfLastSlash))
  }

  private def changePath(url: URL, path: String): URL =
    new URL(url.getProtocol, url.getHost, url.getPort, path, null)

  def loadResource(url: URL): Option[Elem] = {
    try {
      val result = xml.XML.load(url.openStream())
      Some(Utility.trimProper(result).asInstanceOf[Elem])
    } catch {
      case _: FileNotFoundException => None
    }
  }

  def loadNames[K <: WithMetadata[Metadata]](obj: AnyRef, keys: Seq[K], name: String): Map[K, Metadata] = {
    val (className, url) = getUrl(obj)
    bind(keys, loadMetadataResource(url, className, name)).toMap.mapValues { metadata =>
      metadata.attributes.close()
      new NamesOnlyMetadata(metadata.names)
    }
  }

  def getUrl(obj: AnyRef): (String, URL) = {
    val className: String = WithMetadata.className(obj)
    val url = obj.getClass.getResource(className + ".xml")
    (className, url)
  }

  def loadMetadataResource(url: URL, what: String, name: String): Seq[MetadataPreparsed] = {
    val elements: Seq[Elem] = loadMetadata(url, what)
    loadMetadata(url, elements, name)
  }

  def loadMetadata(url: URL, elements: Seq[Elem], name: String): Seq[MetadataPreparsed] =
    elements.map(element => loadSubresource(parentUrl(url), element, name))

  private def loadSubresource(url: URL, element: Elem, name: String ): MetadataPreparsed = {
    val (attributes: Attributes, namesOption: Option[Names], elements: Seq[Elem]) =
      NamesParser.parseNames(element, name)
    val names: Names = namesOption.get

    if (elements.nonEmpty) MetadataPreparsed(attributes, names, elements) else {
      val subresources: Seq[Elem] = names.names.flatMap(name => loadResource(childFileUrl(url, name.name)))
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

  private def loadMetadata(url: URL, what: String): Seq[Elem] = {
    val element = loadResource(url)
    require(element.isDefined, s"No resource: $url")
    val (attributes, elements) = open(element.get, "meta")
    val typeOption = attributes.get("type")
    attributes.close()
    require(typeOption.nonEmpty, "Attribute 'type' is missing.")
    require(typeOption.contains(what), s"Wrong metadata type: ${typeOption.get} instead of $what")
    elements
  }

  def bind[K <: WithMetadata[_], M <: Metadata](keys: Seq[K], metadatas: Seq[M]): Seq[(K, M)] = {
    Names.checkDisjoint(metadatas.map(_.names))
    require(keys.length == metadatas.length)
    // TODO should we give more ordering flexibility - or continue to insist on position correspondence?
    // Weeks parser expects the results in order...
    keys.zip(metadatas).map { case (key, metadata) =>
      require(metadata.names.has(key.name))
      key -> metadata
    }
  }
}
