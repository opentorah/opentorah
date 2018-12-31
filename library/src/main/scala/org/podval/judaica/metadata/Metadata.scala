package org.podval.judaica.metadata

import java.io.FileNotFoundException

import org.podval.judaica.util.Util

import scala.xml.{Elem, Utility}

final case class Metadata(
  attributes: Attributes, // actual parser needs to call close()!
  names: Names,
  elements: Seq[Elem]
) extends WithNames {
  override def toString: String = names.toString
}

object Metadata {
  // This is lazy to allow correct initialization: the code uses values(),
  // Language metadata file references Language instances by name :)
  def loadNames[K <: WithName](
    keys: Seq[K],
    obj: AnyRef,
    resourceName: String
  ): Map[K, Names] = bind(
    keys,
    loadMetadataElements(obj, Some(resourceName), rootElementName = "names", elementName = "names")
      .map(element => Names.parse(element))
  )

  def loadMetadata[K <: WithName, M <: HasName](
    keys: Seq[K],
    obj: AnyRef,
    elementName: String,
    resourceName: Option[String] = None
  ): Map[K, Metadata] = bind(
    keys = keys,
    obj = obj,
    elements = loadMetadataElements(obj,
      resourceName,
      rootElementName = "metadata",
      elementName = elementName)
  )

  def bind[K <: WithName](
    keys: Seq[K],
    elements: Seq[Elem],
    obj: AnyRef
  ): Map[K, Metadata] = bind(keys, elements.map(loadSubresource(_, obj)))

  private def loadSubresource(element: Elem, obj: AnyRef): Metadata = {
    val elementName: String = element.label
    val (attributes, elements) = XML.open(element, elementName)
    attributes.get("resource").fold(Metadata(attributes, elements)) { subresourceName: String =>
      attributes.close()
      val subresource: Elem = loadResource(obj, subresourceName)
      val (newAttributes, newElements) = XML.open(subresource, elementName)
      Metadata(newAttributes, newElements)
    }
  }

  private def apply(attributes: Attributes, elements: Seq[Elem]): Metadata = {
    val (names: Names, tail: Seq[Elem]) = Names.parse(attributes, elements)
    Metadata(attributes, names, tail)
  }

  def loadMetadataElements(
    obj: AnyRef,
    resourceName: Option[String],
    rootElementName: String,
    elementName: String
  ): Seq[Elem] = {
    val resourceNameEffective = resourceName.getOrElse(Util.className(obj))
    val element = loadResource(obj, resourceNameEffective)
    val (attributes, elements) = XML.open(element, rootElementName)
    val type_ = attributes.doGet("type")
    attributes.close()
    require(type_ == resourceNameEffective, s"Wrong metadata type: $type_ instead of $resourceNameEffective")
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
  private def bind[K <: WithName, M <: HasName](keys: Seq[K], metadatas: Seq[M]): Map[K, M] =
    findAndBind(keys, metadatas).toMap

  private def findAndBind[K <: WithName, M <: HasName](keys: Seq[K], metadatas: Seq[M]): Seq[(K, M)] = {
    if (keys.isEmpty) require(metadatas.isEmpty, s"Unmatched metadatas: ${metadatas.mkString("\n")}")
    if (metadatas.isEmpty) require(keys.isEmpty, s"Unmatched keys: $keys")
    Util.checkNoDuplicates(keys, s"keys")

    if (keys.isEmpty) Nil else {
      val key: K = keys.head
      val (withName: Seq[M], withoutName: Seq[M]) = metadatas.partition(_.hasName(key.name))
      require(withName.nonEmpty, s"No metadata for ${key.name}")
      require(withName.length == 1)
      (key, withName.head) +: findAndBind(keys.tail, withoutName)
    }
  }
}
