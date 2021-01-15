package org.opentorah.collector

import org.opentorah.tei.{Author, EntityReference, EntityType, Funder, Principal, RespStmt, Sponsor, Tei, Title, TitleStmt}
import org.opentorah.util.Files
import org.opentorah.xml.{From, Parser, RawXml, Xml}

final class References(references: Seq[EntityReference]) {

  def verify(site: Site): Seq[String] = {
    def checkReference(reference: EntityReference): Option[String] = {
      val name: Seq[Xml.Node] = reference.name
      reference.ref.fold[Option[String]](None) { ref =>
        if (ref.contains(" ")) Some(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
          site.entities.findByName(ref).fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
            if (named.entityType == reference.entityType) None
            else Some(s"${reference.entityType} reference to ${named.entityType} ${named.name}: $name [$ref]")
          }
        }
      }
    }

    references.flatMap(reference => checkReference(reference))
  }

  def noRef: Seq[EntityReference] = references.filter(_.ref.isEmpty).sortBy(_.text.toLowerCase)

  def toId(id: String): Seq[EntityReference] = references.filter(_.ref.contains(id))
}

object References {

  def fromSite(site: Site): Seq[EntityReference] = {
    val fromEntities: Seq[Seq[EntityReference]] =
      for (entity <- site.entities.entities)
        yield addSource(entity.path(site), fromXml(site.entities.getFile(entity).content))

    val fromHierarchy: Seq[Seq[EntityReference]] =
      for (hierarchy <- site.hierarchies)
      yield addSource(hierarchy.path(site), fromHierarchical(hierarchy))

    val fromCollections: Seq[Seq[EntityReference]] =
      for (collection <- site.collections)
      yield addSource(collection.path(site), fromHierarchical(collection))

    val fromDocuments: Seq[Seq[EntityReference]] =
      for {
        collection <- site.collections
        document <- collection.documents
      } yield addSource(collection.textFacet.of(document).path(site), fromTei(collection.getFile(document)))

    (fromEntities ++ fromHierarchy ++ fromCollections ++ fromDocuments).flatten
  }

  private def fromHierarchical(hierarchical: Hierarchical): Seq[EntityReference] =
    fromValues(Some(hierarchical.title), hierarchical.storeAbstract, hierarchical.body)

  private def fromTei(tei:Tei): Seq[EntityReference] = {
    def references(titleStmt: TitleStmt): Seq[EntityReference] = {
      val xml: Seq[Xml.Node] =
        Title.element.seq.unparser.content(titleStmt.titles) ++
        Author.element.seq.unparser.content(titleStmt.authors) ++
        Sponsor.element.seq.unparser.content(titleStmt.sponsors) ++
        Funder.element.seq.unparser.content(titleStmt.funders) ++
        Principal.element.seq.unparser.content(titleStmt.principals) ++
        RespStmt.element.seq.unparser.content(titleStmt.respStmts)

      EntityReference.from(xml) ++ titleStmt.editors.flatMap(_.persName.toSeq)
    }

    // TODO unfold: convert the whole TEI document to XML and!
    // ... and the same everywhere!

    val fromTitleStmt: Seq[EntityReference] = references(tei.titleStmt)

    val fromRest: Seq[EntityReference] = fromValues(
      tei.teiHeader.profileDesc.flatMap(_.documentAbstract), tei.correspDesc, Some(tei.body))

    fromTitleStmt ++ fromRest
  }

  private def fromValues(values: Option[RawXml#Value]*): Seq[EntityReference] =
    fromXml(values.flatten.flatMap(_.xml))

  private def fromXml(xml: Seq[Xml.Node]): Seq[EntityReference] = for {
    entityType <- EntityType.values
    node <- xml
    descendant <- Xml.descendants(node, entityType.nameElement)
  } yield
    // TODO introduce and use here parser that doesn't allow sourceUrl
    Parser.parseDo(EntityReference.parse(From.xml("descendants", descendant)))

  private def addSource(path: Store.Path, references: Seq[EntityReference]): Seq[EntityReference] = {
    val source: Option[String] = Some(Files.mkUrl(path.map(_.structureName)))
    for (reference <- references) yield reference.copy(sourceUrl = source)
  }
}
