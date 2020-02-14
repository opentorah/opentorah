package org.digitaljudaica.metadata

import cats.implicits._
import org.digitaljudaica.util.Collections
import org.digitaljudaica.xml.{Attribute, Element, From, Parser}

object Metadata {

  def load[M](
    from: From,
    elementName: String,
    parser: Parser[M]
  ): Seq[M] = {
    val wrappedParser = wrapped(
      rootElementName = "metadata",
      typeName = from.name,
      elementName = elementName,
      parser = parser)

    from.parseDo(wrappedParser)
  }

  def loadNames[K <: WithName](
    keys: Seq[K],
    from: From
  ): Map[K, Names] = {
    val wrappedParser: Parser[Seq[Names]] = wrapped(
      rootElementName = "names",
      typeName = from.name,
      elementName = "names",
      Names.parser)

    val metadatas: Seq[Names] = from.parseDo(wrappedParser)

    bind(
      keys,
      metadatas,
      (metadata: Names, name: String) => metadata.hasName(name)
    ).toMap
  }

  private def wrapped[A](rootElementName: String, typeName: String, elementName: String, parser: Parser[A]): Parser[Seq[A]] =
    Element.withName(rootElementName, for {
      type_ <- Attribute.required("type")
      _ <- Parser.check(type_ == typeName, s"Wrong metadata type: $type_ instead of $typeName")
      result <- Element.all(elementName, parser)
    } yield result)

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
