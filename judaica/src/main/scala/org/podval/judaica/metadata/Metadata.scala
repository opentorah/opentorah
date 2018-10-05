package org.podval.judaica.metadata

import java.io.FileNotFoundException

import scala.xml.{Elem, Utility}

object Metadata {
  final def load[K <: Named.NamedBase, M <: Named.HasName](
    values: Seq[K],
    obj: AnyRef,
    resourceName: String,
    rootElementName: String,
    elementName: String
  ): Map[K, PreparsedMetadata] = load(
    values,
    obj,
    metadataElements = loadMetadataElements(obj, resourceName, rootElementName),
    elementName
  )

  final def load[K <: Named.NamedBase, M <: Named.HasName](
    values: Seq[K],
    obj: AnyRef,
    metadataElements: Seq[Elem],
    elementName: String
  ): Map[K, PreparsedMetadata] =
    bind(values, metadataElements.map(loadSubresource(obj, _, elementName)))

  final def loadMetadataElements(obj: AnyRef, resourceName: String, rootElementName: String): Seq[Elem] = {
    val element = loadResource(obj, resourceName)
    val (attributes, elements) = XML.open(element, rootElementName)
    val type_ = attributes.doGet("type")
    attributes.close()
    require(type_ == resourceName, s"Wrong metadata type: $type_ instead of $resourceName")
    elements
  }

  final def loadResource(obj: AnyRef, resourceName: String): Elem = {
    val url = Option(obj.getClass.getResource(resourceName + ".xml"))
    val result = url.flatMap { url =>
      try {
        val result = xml.XML.load(url.openStream())
        Some(Utility.trimProper(result).asInstanceOf[Elem])
      } catch {
        case _: FileNotFoundException => None
      }
    }

    require(result.isDefined, s"No resource: $resourceName")
    result.get
  }

  final def loadSubresource(obj: AnyRef, element: Elem, elementName: String): PreparsedMetadata = {
    val (attributes, elements) = XML.open(element, elementName)
    attributes.get("resource").fold(pack(attributes, elements)) { subresourceName: String =>
      attributes.close()
      val subresource: Elem = loadResource(obj, subresourceName)
      val (newAttributes, newElements) = XML.open(subresource, elementName)
      pack(newAttributes, newElements)
    }
  }

  private def pack(attributes: Attributes, elements: Seq[Elem]): PreparsedMetadata = {
    val (names: Names, tail: Seq[Elem]) = Names.parse(attributes, elements)
    PreparsedMetadata(attributes, names, tail)
  }

  final def bind[K <: Named.NamedBase, M <: Named.HasName]( // TODO make M be always PreparsedMetadata?
    keys: Seq[K],
    metadatas: Seq[M]
  ): Map[K, M] = {
    require(keys.length == metadatas.length)

    // TODO disjoint

    // TODO relax the "same order" requirement.
    keys.zip(metadatas).map { case (key, metadata) =>
      require(metadata.hasName(key.name), s"Metadata entry $metadata doesn't have the name ${key.name}")
      key -> metadata
    }.toMap
  }
}
