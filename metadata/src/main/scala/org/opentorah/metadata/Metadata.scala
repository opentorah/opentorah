package org.opentorah.metadata

import org.opentorah.xml.{Attribute, Element, From, Parsable, Parser, Result}

object Metadata {

  def load[M](
    from: From,
    rootElementName: Option[String] = None,
    elementParsable: Parsable[M]
  ): Parser[Seq[M]] = new Element[Seq[M]](rootElementName.getOrElse("metadata")) {
    private val typeName = from.name
    override protected def parser: Parser[Seq[M]] = for {
      type_ <- Attribute("type").required
      _ <- Parser.check(type_ == typeName, s"Wrong metadata type: $type_ instead of $typeName")
      result <- elementParsable.all
    } yield result
  }.parse(from)

  def loadNames[K <: WithName](
    obj: AnyRef,
    resourceName: String,
    keys: Seq[K]
  ): Parser[Map[K, Names]] = for {
    metadatas <- load(
      from = From.resource(obj, resourceName),
      rootElementName = Some("names"),
      elementParsable = new Element[Names]("names") {
        override protected def parser: Parser[Names] = Names.withoutDefaultNameParser
      }
    )

    result <- bind(
      keys,
      metadatas,
      hasName = (metadata: Names, name: String) => metadata.hasName(name)
    )
  } yield result.toMap

  def bind[K <: WithName, M](
    keys: Seq[K],
    metadatas: Seq[M],
    hasName: (M, String) => Boolean
  ): Parser[Seq[(K, M)]] = bind(
    keys,
    getName = (key: WithName) => key.name,
    metadatas,
    hasName
  )

  private def bind[K, M](
    keys: Seq[K],
    getName: K => String,
    metadatas: Seq[M],
    hasName: (M, String) => Boolean
  ): Parser[Seq[(K, M)]] = for {
    result <- Parser.collectAll(metadatas.map(metadata => find(keys, getName, metadata, hasName).map(_ -> metadata)))
    _ <- checkNoUnmatchedKeys(keys.toSet -- result.map(_._1).toSet)
  } yield result

  def bind[K, M](
    keys: Seq[K],
    metadatas: Seq[M],
    getKey: M => K
  ): Parser[Map[K, M]] = {
    val result = metadatas.map(metadata => getKey(metadata) -> metadata).toMap
    for {
      _ <- checkNoUnmatchedKeys(keys.toSet -- result.keySet)
    } yield result
  }

  private def checkNoUnmatchedKeys[K](unmatchedKeys: Set[K]): Result =
    Parser.check(unmatchedKeys.isEmpty, s"Unmatched keys: $unmatchedKeys")

  def find[K <: WithName](
    keys: Seq[K],
    names: Names
  ): Parser[K] = find(
    keys = keys,
    getName = (key: K) => key.name,
    metadata = names,
    hasName = (names: Names, name: String) => names.hasName(name)
  )

  private def find[K, M](
    keys: Seq[K],
    getName: K => String,
    metadata: M,
    hasName: (M, String) => Boolean
  ): Parser[K] = {
    val result: Seq[K] = keys.filter(key => hasName(metadata, getName(key)))
    for {
      _ <- Parser.check(result.nonEmpty, s"Unmatched metadata: $metadata")
      _ <- Parser.check(result.length == 1, s"Metadata matched multiple keys: $metadata")
    } yield result.head
  }
}
