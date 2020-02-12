package org.digitaljudaica.xml

import cats.implicits._
import org.digitaljudaica.metadata.{HasName, Names, WithName}
import org.digitaljudaica.util.Collections

object Load {

  def names[K <: WithName](
     keys: Seq[K],
     from: From
  ): Map[K, Names] = {
    val wrappedParser = wrapped(
      rootElementName = "names",
      typeName = from.name,
      elementName = "names",
      Names.parser)
    bind(
      keys,
      from.parseDo(wrappedParser)
    )
  }

  def metadataUsingNames[K <: WithName, M](
    keys: Seq[K],
    from: From,
    elementName: String,
    parser: Parser[M]
  ): Map[K, M] = {
    val result = findAndBind(
      keys,
      metadata(from, elementName, Names.withNames(parser)),
      (metadata: (Names, M), name: String) => metadata._1.hasName(name)
    ).toMap
    Collections.mapValues(result)(_._2)
  }

  def metadata[M](
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

  def wrapped[A](rootElementName: String, typeName: String, elementName: String, parser: Parser[A]): Parser[Seq[A]] =
    Element.withName(rootElementName, for {
      type_ <- Attribute.required("type")
      _ <- Parser.check(type_ == typeName, s"Wrong metadata type: $type_ instead of $typeName")
      result <- Element.all(elementName, parser)
    } yield result)

  def bind[K <: WithName, M <: HasName](keys: Seq[K], metadatas: Seq[M]): Map[K, M] =
    findAndBind(keys, metadatas, (metadata: M, name: String) => metadata.hasName(name)).toMap

  private def findAndBind[K <: WithName, M](keys: Seq[K], metadatas: Seq[M], hasName: (M, String) => Boolean): Seq[(K, M)] = {
    if (keys.isEmpty) require(metadatas.isEmpty, s"Unmatched metadatas: ${metadatas.mkString("\n")}")
    if (metadatas.isEmpty) require(keys.isEmpty, s"Unmatched keys: $keys")
    Collections.checkNoDuplicates(keys, s"keys")

    if (keys.isEmpty) Nil else {
      val key: K = keys.head
      val (withName: Seq[M], withoutName: Seq[M]) = metadatas.partition(metadata => hasName(metadata, key.name))
      require(withName.nonEmpty, s"No metadata for ${key.name}")
      require(withName.length == 1)
      (key, withName.head) +: findAndBind(keys.tail, withoutName, hasName)
    }
  }
}
