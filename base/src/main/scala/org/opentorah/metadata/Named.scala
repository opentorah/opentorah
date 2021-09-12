package org.opentorah.metadata

import org.opentorah.util.{Effects, Util}
import org.opentorah.xml.{Elements, From, Parser}
import zio.IO

trait Named:
  def name: String = Util.className(this)

  override def toString: String = name

  def names: Names

  final def structureName: String = names.doFind(Language.English.toSpec).name

  def merge(that: Named): Named =
    require(this == that)
    this

  final def toLanguageString(using spec: LanguageSpec): String = names.toLanguageString(using spec)

object Named:

  def load[K <: Named, M](
    from: From,
    content: Elements[M],
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
    content: Elements[M]
  ): Parser[Seq[M]] = content.wrappedSeq(from.name).parse(from)

  def bind[K <: Named, M](
    keys: Seq[K],
    metadatas: Seq[M],
    getKey: M => K
  ): Parser[Map[K, M]] =
    val result: Map[K, M] = metadatas.map(metadata => getKey(metadata) -> metadata).toMap
    for
      _ <- checkNoUnmatchedKeys(keys.toSet -- result.keySet)
    yield result

  private def checkNoUnmatchedKeys[K](unmatchedKeys: Set[K]): IO[Effects.Error, Unit] =
    Effects.check(unmatchedKeys.isEmpty, s"Unmatched keys: $unmatchedKeys")

  def find[K <: Named](
    keys: Seq[K],
    names: Names
  ): Parser[K] = find(
    keys = keys,
    metadata = names,
    hasName = (names: Names, name: String) => names.hasName(name)
  )

  private def find[K <: Named, M](
    keys: Seq[K],
    metadata: M,
    hasName: (M, String) => Boolean
  ): Parser[K] =
    val result: Seq[K] = keys.filter(key => hasName(metadata, key.name))
    for
      _ <- Effects.check(result.nonEmpty, s"Unmatched metadata: $metadata")
      _ <- Effects.check(result.length == 1, s"Metadata matched multiple keys: $metadata")
    yield result.head
