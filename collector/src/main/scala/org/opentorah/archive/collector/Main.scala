package org.opentorah.archive.collector

import java.io.File
import java.net.URL
import org.opentorah.entity.{EntitiesList, Entity, EntityReference}
import org.opentorah.store.{Store, WithPath}
import org.opentorah.util.Files
import org.opentorah.xml.{From, PaigesPrettyPrinter}

object Main {

  def main(args: Array[String]): Unit = {
    doIt(args(0))
  }

  def doIt(docsStr: String): Unit = {
    println("Reading store.")

    // TODO separate URL for store and directory for site!
    val docs: File = new File(docsStr)
    val fromUrl: URL = From.file(new File(new File(docs, "store"), "store.xml")).url.get
    val store: Store = Store.read(fromUrl)

    val lists: Seq[EntitiesList] = store.entities.get.lists
    val entities: Seq[Entity] = store.entities.get.by.get.stores.map(_.entity)
    val collections: Seq[WithPath[Collection]] = Util.getCollections(store)
    val references: Seq[WithPath[EntityReference]] = store.withPath[EntityReference](values = _.references)

    println("Checking store.")
    def findByRef(ref: String): Option[Entity] = store.entities.get.findByRef(ref)
    val errors: Seq[String] = references.flatMap(reference => checkReference(reference.value, findByRef))
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))

    println("Pretty-printing store.")
    // TODO do translations also!
    // TODO remove common stuff (calendarDescriptor etc.)
    // TODO closing tag should stick to preceding characters! see niab 611:
    //   ... доходов</title>
    prettyPrint(store, Util.teiPrettyPrinter)

    Site.write(
      docs,
      store,
      lists,
      entities,
      references
    )
  }

  private def prettyPrint(store: Store, prettyPrinter: PaigesPrettyPrinter): Unit = {
    if (store.fromUrl.isDefined && Files.isFile(store.fromUrl.get)) Util.teiPrettyPrinter.writeXml(
      file = Files.url2file(store.fromUrl.get),
      elem = store.toXml
    )
    if (store.entities.isDefined)
      prettyPrint(store.entities.get, prettyPrinter)
    if (store.by.isDefined) // TODO get rid of the cast:
      store.by.get.stores.foreach[Unit] { store => prettyPrint(store.asInstanceOf[Store], prettyPrinter) }
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
