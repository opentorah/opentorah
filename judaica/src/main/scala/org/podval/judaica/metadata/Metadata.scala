package org.podval.judaica.metadata

import java.io.FileNotFoundException

import scala.xml.{Elem, Utility}

final case class Metadata(
  attributes: Attributes, // actual parser needs to call close()!
  names: Names,
  elements: Seq[Elem]
) extends WithNames

object Metadata {
  // This is lazy to allow correct initialization: the code uses values(),
  // Language metadata file references Language instances by name :)
  def loadNames[K <: Named](
    keys: Seq[K],
    obj: AnyRef,
    resourceName: String
  ): Map[K, Names] = bind(
    keys,
    loadMetadataElements(obj, resourceName, rootElementName = "names", elementName = "names")
      .map(element => Names.parse(element, None))
  )

  def loadMetadata[K <: Named, M <: HasName](
    keys: Seq[K],
    obj: AnyRef,
    resourceName: String,
    rootElementName: String,
    elementName: String
  ): Map[K, Metadata] = bind(
    keys = keys,
    elements = loadMetadataElements(obj, resourceName, rootElementName = "metadata", elementName = elementName),
    obj = obj
  )

  final def bind[K <: Named](
    keys: Seq[K],
    elements: Seq[Elem],
    obj: AnyRef
  ): Map[K, Metadata] = {
    def loadSubresource(element: Elem): Metadata = {
      val elementName: String = element.label
      val (attributes, elements) = XML.open(element, elementName)
      attributes.get("resource").fold(Metadata(attributes, elements)) { subresourceName: String =>
        attributes.close()
        val subresource: Elem = loadResource(obj, subresourceName)
        val (newAttributes, newElements) = XML.open(subresource, elementName)
        Metadata(newAttributes, newElements)
      }
    }

    bind(keys, elements.map(loadSubresource))
  }

  private def apply(attributes: Attributes, elements: Seq[Elem]): Metadata = {
    val (names: Names, tail: Seq[Elem]) = Names.parse(attributes, elements)
    Metadata(attributes, names, tail)
  }

  private def loadMetadataElements(
    obj: AnyRef,
    resourceName: String,
    rootElementName: String,
    elementName: String
  ): Seq[Elem] = {
    val element = loadResource(obj, resourceName)
    val (attributes, elements) = XML.open(element, rootElementName)
    val type_ = attributes.doGet("type")
    attributes.close()
    require(type_ == resourceName, s"Wrong metadata type: $type_ instead of $resourceName")
    XML.span(elements, elementName)
  }

  private def loadResource(obj: AnyRef, resourceName: String): Elem = {
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

  // This is used to bind both Metadata and Names, so - HasName.
  private def bind[K <: Named, M <: HasName](keys: Seq[K], metadatas: Seq[M]): Map[K, M] = {
    require(keys.length == metadatas.length)

    // TODO check that the names are disjoint

    findAndBind(keys, metadatas).toMap
  }


  private def findAndBind[K <: Named, M <: HasName](keys: Seq[K], metadatas: Seq[M]): Seq[(K, M)] = {
    require(keys.isEmpty == metadatas.isEmpty)
    if (keys.isEmpty) Nil else {
      val key: K = keys.head
      val (withName: Seq[M], withoutName: Seq[M]) = metadatas.partition(_.hasName(key.name))
      require(withName.nonEmpty)
      require(withName.length == 1)
      (key, withName.head) +: findAndBind(keys.tail, withoutName)
    }
  }
}
