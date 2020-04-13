package org.opentorah.collector

import java.io.File
import java.net.URL
import org.opentorah.entity.{Entity, EntityReference}
import org.opentorah.store.{Store, WithPath}
import org.opentorah.tei.Tei
import org.opentorah.util.{Files, Xml}
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
    for (entityHolder <- store.entities.get.by.get.stores)
      prettyPrint(entityHolder, Transformations.teiPrettyPrinter, Entity.toXml(entityHolder.entity.copy(id = None)))
    prettyPrint(store, Transformations.teiPrettyPrinter)

    val site = new Site(store, references)

    Site.write(
      siteRoot,
      site
    )
  }

  private def prettyPrint(store: Store, prettyPrinter: PaigesPrettyPrinter): Unit = {
    prettyPrint(store, prettyPrinter, Store.parsable.toXml(store.asInstanceOf[Store.FromElement].element))

    store match {
      case collection: Collection =>
        for (by <- collection.by; document <- by.stores; by <- document.by; teiHolder <- by.stores)
          prettyPrint(teiHolder, prettyPrinter, Tei.toXml(teiHolder.tei))
      case _ =>
        for (by <- store.by; store <- by.stores) prettyPrint(store.asInstanceOf[Store], prettyPrinter)
    }
  }

  private def prettyPrint(store: Store, prettyPrinter: PaigesPrettyPrinter, toXml: => Elem): Unit =
    for (fromUrl <- store.urls.fromUrl) if (Files.isFile(fromUrl)) Files.write(
      file = Files.url2file(fromUrl),
      content = Xml.xmlHeader + prettyPrinter.render(toXml) + "\n"
    )

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
