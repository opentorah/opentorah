package org.opentorah.archive.collector

import java.io.File
import java.net.URL
import org.opentorah.entity.{EntitiesList, Entity, EntityReference}
import org.opentorah.store.{Store, WithPath}
import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.From

object Main {

  def main(args: Array[String]): Unit = {
    doIt(args(0))
  }

  def doIt(docsStr: String): Unit = {
    println("Reading store.")

    // TODO separate URL for store and directory for site!
    val docs: File = new File(docsStr)
    val url: URL = From.file(new File(new File(docs, "store"), "store.xml")).url.get
    val store: Store = Store.fromUrl(url)

    val lists: Seq[EntitiesList] = store.entities.get.lists
    val entities: Seq[Entity] = store.entities.get.by.stores.map(_.entity)
    val collections: Seq[WithPath[Collection]] = Util.getCollections(store)
    val references: Seq[WithPath[EntityReference]] = store.withPath[EntityReference](values = _.references)

    println("Checking store.")
    def findByRef(ref: String): Option[Entity] = store.entities.get.findByRef(ref)
    val errors: Seq[String] = references.flatMap(reference => checkReference(reference.value, findByRef))
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))

    println("Pretty-printing store.")
    // TODO do translations also!
    // TODO only if the URL is a file URL
    // TODO remove common stuff (calendarDescriptor etc.)
    for {
      collection <- collections
      document <- collection.value.documents
    } Util.teiPrettyPrinter.writeXml(
      Files.url2file(document.url),
      Tei.toXml(document.tei)
    )

    // TODO only if the URL is a file URL
    for (entityStore <- store.entities.get.by.stores) Util.teiPrettyPrinter.writeXml(
      Files.url2file(entityStore.url),
      Entity.toXml(entityStore.entity.copy(id = None))
    )

    Site.write(
      docs,
      store,
      lists,
      entities,
      references
    )
  }

  private def checkReference(reference: EntityReference,  findByRef: String => Option[Entity]): Option[String] = {
    val name = reference.name
    reference.ref.fold[Option[String]](None) { ref =>
      if (ref.contains(" ")) Some(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
        findByRef(ref).fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
          if (named.entityType == reference.entityType) None
          else Some(s"${reference.entityType} reference to ${named.entityType} ${named.name}: $name [$ref]")
        }
      }
    }
  }
}
