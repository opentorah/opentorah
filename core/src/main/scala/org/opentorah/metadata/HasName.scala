package org.opentorah.metadata

import org.opentorah.util.{ClassName, Effects}

trait HasName(nameOverride: Option[String]):
  final def name: String = nameOverride.getOrElse(defaultName)

  protected def defaultName: String

object HasName:

  trait Enum extends HasName:
    self: HasName =>
    final override protected def defaultName: String = this.toString

  trait NonEnum extends HasName:
    self: HasName =>
    final override protected def defaultName: String = ClassName.get(this)

  def mapByName[K <: HasName, M](
    keys: Seq[K],
    metadatas: Seq[M],
    hasName: (M, String) => Boolean
  ): Effects.IO[Map[K, M]] = for
    result: Seq[(K, M)] <- Effects.collectAll(metadatas.map(metadata =>
      find(
        keys,
        metadata,
        hasName
      ).map(_ -> metadata)
    ))
    _ <- checkNoUnmatchedKeys(keys.toSet -- result.map(_._1).toSet)
  yield result.toMap

  def bind[K, M](
    keys: Seq[K],
    metadatas: Seq[M],
    getKey: M => K
  ): Effects.IO[Map[K, M]] =
    val result: Map[K, M] = metadatas.map(metadata => getKey(metadata) -> metadata).toMap
    for
      _ <- checkNoUnmatchedKeys(keys.toSet -- result.keySet)
    yield result

  def find[K <: HasName](
    keys: Seq[K],
    names: Names
  ): Effects.IO[K] = find(
    keys = keys,
    metadata = names,
    hasName = (names: Names, name: String) => names.hasName(name)
  )

  def findByNames[K <: HasName](keys: Seq[K], names: Names): K =
    val result: Seq[K] = keys.filter(key => names.hasName(key.name))
    require(result.nonEmpty, s"Unmatched metadata: $names")
    require(result.length == 1, s"Metadata matched multiple keys: $names")
    result.head

  private def find[K <: HasName, M](
    keys: Seq[K],
    metadata: M,
    hasName: (M, String) => Boolean
  ): Effects.IO[K] =
    val result: Seq[K] = keys.filter(key => hasName(metadata, key.name))
    for
      _ <- Effects.check(result.nonEmpty, s"Unmatched metadata: $metadata")
      _ <- Effects.check(result.length == 1, s"Metadata matched multiple keys: $metadata")
    yield result.head

  private def checkNoUnmatchedKeys[K](unmatchedKeys: Set[K]): Effects.IO[Unit] =
    Effects.check(unmatchedKeys.isEmpty, s"Unmatched keys: $unmatchedKeys")
