package org.digitaljudaica.metadata

import cats.implicits._
import org.digitaljudaica.util.Collections
import org.digitaljudaica.xml.{From, Parser, Xml}

object Metadata {

  def load[M](
    from: From,
    rootElementName: Option[String] = None,
    elementName: String,
    parser: Parser[M]
  ): Seq[M] = {
    val typeName = from.name

    val wrappedParser = for {
      type_ <- Xml.attribute.required("type")
      _ <- Parser.check(type_ == typeName, s"Wrong metadata type: $type_ instead of $typeName")
      result <- Xml.element.elements.all(elementName, parser)
    } yield result

    from.elements.parseDo(Xml.withName(rootElementName.getOrElse("metadata"), wrappedParser))
  }

  def loadNames[K <: WithName](
    keys: Seq[K],
    from: From
  ): Map[K, Names] = {
    val metadatas: Seq[Names] = load(
      from,
      rootElementName = Some("names"),
      elementName = "names",
      parser = Names.parser
    )

    bind(
      keys,
      metadatas,
      (metadata: Names, name: String) => metadata.hasName(name)
    ).toMap
  }

  def bind[K <: WithName, M](
    keys: Seq[K],
    metadatas: Seq[M],
    hasName: (M, String) => Boolean
  ): Seq[(K, M)] = bind(
    keys,
    (key: WithName) => key.name,
    metadatas,
    hasName
  )

  def bind[K, M](
    keys: Seq[K],
    getName: K => String,
    metadatas: Seq[M],
    hasName: (M, String) => Boolean
  ): Seq[(K, M)] = {
    if (keys.isEmpty) require(metadatas.isEmpty, s"Unmatched metadatas: ${metadatas.mkString("\n")}")
    if (metadatas.isEmpty) require(keys.isEmpty, s"Unmatched keys: $keys")
    Collections.checkNoDuplicates(keys, s"keys")

    // TODO rework to look for key and reuse find() below...
    if (keys.isEmpty) Nil else {
      val key: K = keys.head
      val (withName: Seq[M], withoutName: Seq[M]) = metadatas.partition(metadata => hasName(metadata, getName(key)))
      require(withName.nonEmpty, s"No metadata for ${getName(key)}")
      require(withName.length == 1)
      (key, withName.head) +: bind(keys.tail, getName, withoutName, hasName)
    }
  }

  def find[K <: WithName, M <: HasName](keys: Seq[K], metadata: M): K = find(
    keys = keys,
    getName = (key: K) => key.name,
    metadata = metadata,
    hasName = (metadata: M, name: String) => metadata.hasName(name)
  )

  def find[K, M](
    keys: Seq[K],
    getName: K => String,
    metadata: M,
    hasName: (M, String) => Boolean
  ): K = {
    val result: Seq[K] = keys.filter(key => hasName(metadata, getName(key)))
    require(result.nonEmpty, s"Unmatched metadata $metadata")
    require(result.length == 1, s"Metadata matched multiple keys: $metadata")
    result.head
  }

  def toMap[K, M](keys: Seq[K], metadatas: Seq[M], getKey: M => K): Map[K, M] = {
    // TODO check that all keys are bound
    metadatas.map(metadata => getKey(metadata) -> metadata).toMap
  }
}