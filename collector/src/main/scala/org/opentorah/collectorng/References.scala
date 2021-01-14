package org.opentorah.collectorng

import org.opentorah.tei.{EntityReference, EntityType, Tei}
import org.opentorah.xml.{From, Parser, RawXml, Xml}

final class References(references: Seq[EntityReference]) {

  def verify(site: Site): Seq[String] = {
    def checkReference(reference: EntityReference): Option[String] = {
      val name: Seq[Xml.Node] = reference.name
      reference.ref.fold[Option[String]](None) { ref =>
        if (ref.contains(" ")) Some(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
          site.byEntity.findByName(ref).fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
            if (named.entityType == reference.entityType) None
            else Some(s"${reference.entityType} reference to ${named.entityType} ${named.name}: $name [$ref]")
          }
        }
      }
    }

    references.flatMap(reference => checkReference(reference))
  }

  //  def noRef: Seq[ReferenceWithSource] =
//    references  // from documents!
//      .filter(_.reference.ref.isEmpty)
//      .sortBy(_.reference.text.toLowerCase)
//
//  def toId(id: String): (Seq[ReferenceWithSource], Seq[ReferenceWithSource]) = {
//    val result: Seq[ReferenceWithSource] = references.filter(_.reference.ref.contains(id))
//    (References.fromEntity(result), References.fromDocument(result))
//  }
}

object References {

  def fromSite(site: Site): Seq[EntityReference] = {
    val fromEntities: Seq[Seq[EntityReference]] =
      for (entity <- site.byEntity.entities)
      yield fromXml(Seq(site.byEntity, entity), site.byEntity.getFile(entity).content)

    fromEntities.flatten ++ fromByHierarchy(Seq.empty, site.by)

    // TODO from notes - if resolves to an Entity
  }

  private def fromByHierarchy(path: Store.Path, by: ByHierarchy): Seq[EntityReference] = by.stores.flatMap {
    case hierarchy: Hierarchy =>
      val hierarchyPath: Store.Path = path ++ Seq(by, hierarchy)
      val fromHierarchy: Seq[EntityReference] = fromValues(hierarchyPath,
        hierarchy.title, hierarchy.storeAbstract, hierarchy.body)
      val fromChildren: Seq[EntityReference] = hierarchy.by.toSeq.flatMap(by => fromByHierarchy(hierarchyPath, by))

      fromHierarchy ++ fromChildren

    case collection: Collection =>
      val collectionPath: Store.Path =
        collection.alias.fold(path ++ Seq(by, collection))(_ => Seq(new CollectionAlias(collection)))
      val fromCollection: Seq[EntityReference] = fromValues(collectionPath,
        collection.title, collection.storeAbstract, collection.body)

      val facet: Collection.HtmlFacet = collection.htmlFacet
      val fromDocuments: Seq[Seq[EntityReference]] =
        for (document <- collection.documents)
        yield fromTei(collectionPath ++ Seq(facet, facet.of(document)), collection.getFile(document))

      fromCollection ++ fromDocuments.flatten

    case _ => Seq.empty
  }

  private def fromTei(path: Store.Path, tei:Tei): Seq[EntityReference] = {
    // TODO unfold: convert the whole TEI document to XML and!
    // ... and the same everywhere!

    val fromTitleStmt: Seq[EntityReference] =
      for (reference <- tei.titleStmt.references) yield reference.copy(sourceUrl = toUrl(path))

    val fromRest: Seq[EntityReference] = fromValues(path,
      tei.getAbstract, tei.correspDesc, Some(tei.body))

    fromTitleStmt ++ fromRest
  }

  private def fromValues(path: Store.Path, values: Option[RawXml#Value]*): Seq[EntityReference] =
    fromXml(path, values.flatten.flatMap(_.xml))

  private def fromXml(path: Store.Path, xml: Seq[Xml.Node]): Seq[EntityReference] = for {
    entityType <- EntityType.values
    node <- xml
    descendant <- Xml.descendants(node, entityType.nameElement)
    from = From.xml("descendants", descendant)
    // TODO introduce and use here parser that doesn't allow sourceUrl
    reference = Parser.parseDo(EntityReference.parse(from))
  } yield reference.copy(sourceUrl = toUrl(path))

  private def toUrl(path: Store.Path): Option[String] = Some(Store.toEnglishUrl(path))
}
