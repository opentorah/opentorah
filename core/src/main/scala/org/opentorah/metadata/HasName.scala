package org.opentorah.metadata

import org.opentorah.platform.Platform
import org.opentorah.util.Effects
import org.opentorah.xml.{ElementsTo, From, Parser}

trait HasName(nameOverride: Option[String]):
  final def name: String = nameOverride.getOrElse(defaultName)

  protected def defaultName: String

object HasName:

  trait Enum extends HasName:
    self: HasName =>
    final override protected def defaultName: String = this.toString

  trait NonEnum extends HasName:
    self: HasName =>
    final override protected def defaultName: String = Platform.className(this)

  def load[K <: HasName, M](
    from: From,
    content: ElementsTo[M],
    keys: Seq[K],
    hasName: (M, String) => Boolean
  ): Parser[Map[K, M]] = for
    metadatas: Seq[M] <- load[M](from, content)
    result: Seq[(K, M)] <- Effects.collectAll(metadatas.map(metadata =>
      find(
        keys,
        metadata,
        hasName
      ).map(_ -> metadata)
    ))
    _ <- checkNoUnmatchedKeys(keys.toSet -- result.map(_._1).toSet)
  yield result.toMap

  def load[M](
    from: From,
    content: ElementsTo[M]
  ): Parser[Seq[M]] = content.wrappedSeq(from.name).parse(from)

  def bind[K, M](
    keys: Seq[K],
    metadatas: Seq[M],
    getKey: M => K
  ): Parser[Map[K, M]] =
    val result: Map[K, M] = metadatas.map(metadata => getKey(metadata) -> metadata).toMap
    for
      _ <- checkNoUnmatchedKeys(keys.toSet -- result.keySet)
    yield result

  def find[K <: HasName](
    keys: Seq[K],
    names: Names
  ): Parser[K] = find(
    keys = keys,
    metadata = names,
    hasName = (names: Names, name: String) => names.hasName(name)
  )

  private def find[K <: HasName, M](
    keys: Seq[K],
    metadata: M,
    hasName: (M, String) => Boolean
  ): Parser[K] =
    val result: Seq[K] = keys.filter(key => hasName(metadata, key.name))
    for
      _ <- Effects.check(result.nonEmpty, s"Unmatched metadata: $metadata")
      _ <- Effects.check(result.length == 1, s"Metadata matched multiple keys: $metadata")
    yield result.head

  private def checkNoUnmatchedKeys[K](unmatchedKeys: Set[K]): Effects.IO[Unit] =
    Effects.check(unmatchedKeys.isEmpty, s"Unmatched keys: $unmatchedKeys")
