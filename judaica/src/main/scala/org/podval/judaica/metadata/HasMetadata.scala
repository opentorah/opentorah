package org.podval.judaica.metadata

import java.io.FileNotFoundException

import scala.xml.{Elem, Utility}

trait HasMetadata extends Named {

  type BindableMetadata <: Named.HasName

  type Metadata

  protected def toMetadata: Key => Metadata

  protected final def loadResource(resourceName: String): Elem = {
    val url = Option(this.getClass.getResource(resourceName + ".xml"))
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

  protected final def bind(
    keys: Seq[Key],
    metadatas: Seq[BindableMetadata],
    parse: (Key, BindableMetadata) => Metadata
  ): Map[Key, Metadata] = {
    require(keys.length == metadatas.length)

    // TODO disjoint

    // TODO relax the "same order" requirement.
    keys.zip(metadatas).map { case (key, metadata) =>
      require(metadata.hasName(key.name), s"Metadata entry $metadata doesn't have the name ${key.name}")
      key -> parse(key, metadata)
    }.toMap
  }
}
