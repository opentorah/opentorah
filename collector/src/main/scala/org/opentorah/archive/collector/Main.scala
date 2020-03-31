package org.opentorah.archive.collector

import java.io.File
import java.net.URL
import org.opentorah.entity.{Entity, EntityReference}
import org.opentorah.store.{EntityHolder, Store, StoreElement, WithPath}
import org.opentorah.tei.Tei
import org.opentorah.util.Files
import org.opentorah.xml.{From, PaigesPrettyPrinter}
import scala.xml.Elem

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    doIt(
      fromUrl = From.file(new File(new File(docs, "store"), "store.xml")).url.get,
      siteRoot = docs
    )
  }

  def doIt(
    fromUrl: URL,
    siteRoot: File
  ): Unit = {
    println("Reading store.")

    val store: Store = Store.read(fromUrl)
    val references: Seq[WithPath[EntityReference]] = store.withPath[EntityReference](values = _.references)

    println("Checking store.")
    def findByRef(ref: String): Option[Entity] = store.entities.get.findByRef(ref)
    val errors: Seq[String] = references.flatMap(reference => checkReference(reference.value, findByRef))
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))

    println("Pretty-printing store.")
    prettyPrint(store, TeiUtil.teiPrettyPrinter)

    Site.write(
      siteRoot,
      store,
      references
    )
  }

  private def prettyPrint(store: Store, prettyPrinter: PaigesPrettyPrinter): Unit = {
    for (fromUrl <- store.fromUrl) if (Files.isFile(fromUrl)) TeiUtil.teiPrettyPrinter.writeXml(
      file = Files.url2file(fromUrl),
      elem = toXml(store)
    )

    for (entities <- store.entities) prettyPrint(entities, prettyPrinter)

    // TODO get rid of the cast:
    for (by <- store.by; store <- by.stores) prettyPrint(store.asInstanceOf[Store], prettyPrinter)
  }

  private def toXml(store: Store): Elem = store match {
    case fromElement: Store.FromElement => StoreElement.toXml(fromElement.element)
    case entityHolder: EntityHolder => Entity.toXml(entityHolder.entity.copy(id = None))
    case teiHolder: TeiHolder => Tei.toXml(TeiUtil.removeCommon(teiHolder.tei))
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
