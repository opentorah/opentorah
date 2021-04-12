package org.opentorah.metadata

import org.opentorah.util.Effects
import org.opentorah.xml.{Element, Elements, From, Parsable, Parser}
import zio.IO

final class Metadata[M](
  elementName: String,
  content: Elements[M]
) extends Element[Seq[M]](elementName) {
  override def contentParsable: Parsable[Seq[M]] = content.seq
}

object Metadata {

  def load[K <: WithName, M](
    from: From,
    content: Elements[M],
    keys: Seq[K],
    hasName: (M, String) => Boolean
  ): Parser[Map[K, M]] = for {
    metadatas <- load[M](from, content)

    result <- bind(
      keys,
      getName = (key: WithName) => key.name,
      metadatas,
      hasName
    )
  } yield result.toMap

  def loadResource[M](
    obj: AnyRef,
    content: Elements[M]
  ): Parser[Seq[M]] = Metadata.load(from = From.resource(obj), content)

  def load[M](
    from: From,
    content: Elements[M]
  ): Parser[Seq[M]] = new Metadata[M](
    elementName = from.name,
    content = content
  ).parse(from)

  private def bind[K, M](
    keys: Seq[K],
    getName: K => String,
    metadatas: Seq[M],
    hasName: (M, String) => Boolean
  ): Parser[Seq[(K, M)]] = for {
    result <- Effects.collectAll(metadatas.map(metadata => find(keys, getName, metadata, hasName).map(_ -> metadata)))
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

  private def checkNoUnmatchedKeys[K](unmatchedKeys: Set[K]): IO[Effects.Error, Unit] =
    Effects.check(unmatchedKeys.isEmpty, s"Unmatched keys: $unmatchedKeys")

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
      _ <- Effects.check(result.nonEmpty, s"Unmatched metadata: $metadata")
      _ <- Effects.check(result.length == 1, s"Metadata matched multiple keys: $metadata")
    } yield result.head
  }
}
