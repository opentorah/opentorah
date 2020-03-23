package org.opentorah.store

import java.net.URL
import org.opentorah.entity.{Entity, EntityReference}
import org.opentorah.metadata.{Language, Name, Names}

final class EntityStore(
  parent: Option[Store],
  url: URL,
  val entity: Entity
) extends Store(parent, url) {

  override def names: Names = {
    val russianName = entity.name
    val englishName = entity.id.get
    new Names(
      Seq(org.opentorah.metadata.Name(russianName, Language.Russian)) ++
      (if (englishName == russianName) Seq.empty else Seq(Name(englishName, Language.English)))
    )
  }

  override def references(at: Path): Seq[EntityReference] =
    entity.references(at)
}
