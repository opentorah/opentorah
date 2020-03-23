package org.opentorah.archive.collector

import java.io.File
import org.opentorah.metadata.Language
import org.opentorah.reference.{Named, Reference}
import org.opentorah.store.Path
import org.opentorah.xml.From

object Main {

  def main(args: Array[String]): Unit = {
    val docs: File = new File(args(0))
    println(s"docs: $docs.")

    println("Reading store.")
    val store: Store = Store.read(From.file(new File(new File(docs, "store"), "store.xml")))

    val namesSelector = store.nameds.get.selector
    val lists = store.nameds.get.lists
    val nameds = store.nameds.get.by.stores
    val caseSelector = store.by.get.selector
    val collections: Seq[Collection] = store.by.get.stores
    val references: Seq[Reference] = store.references(Path.empty)

    println("Checking store.")
    val errors: Seq[String] = checkReferences(references, store.nameds.get.findByRef)
    if (errors.nonEmpty) throw new IllegalArgumentException(errors.mkString("\n"))

    println("Pretty-printing store.")
    store.prettyPrint()

    println("Writing site.")
    val site: Site = new Site(docs)
    site.write(
      namesSelector,
      lists,
      nameds,
      caseSelector,
      collections,
      references
    )

    println("Writing reports.")
    site.writeReport(
      name = "misnamed-nameds",
      title = "Неправильно названные файлы с именами",
      content = misnamedNamedsReport(store.nameds.get.by.stores)
    )

    site.writeReport(
      name = "no-refs",
      title = "Имена без атрибута 'ref'",
      content = noRefsReport(references)
    )
  }

  private def checkReferences(references: Seq[Reference], findByRef: String => Option[Named]): Seq[String] = {
    def check(reference: Reference): Option[String] = {
      val name = reference.name
      reference.ref.fold[Option[String]](None) { ref =>
        if (ref.contains(" ")) Some(s"""Value of the ref attribute contains spaces: ref="$ref" """) else {
          findByRef(ref).fold[Option[String]](Some(s"""Unresolvable reference: Name ref="$ref">${name.text}< """)) { named =>
            if (named.entity != reference.entity) Some(s"${reference.entity} reference to ${named.entity} ${named.name}: $name [$ref]")
            else None
          }
        }
      }
    }

    references.flatMap(check)
  }

  private def misnamedNamedsReport(nameds: Seq[Named]): Seq[String] = nameds.flatMap { named =>
    val id = named.id.get
    val expectedId = named.name.replace(' ', '_')
    if (id == expectedId) None else Some(s"- '$id' должен по идее называться '$expectedId'")
  }

  private def noRefsReport(references: Seq[Reference]): Seq[String] = for (reference <- references.filter(_.ref.isEmpty)) yield
    "- " + reference.name.map(_.text.trim).mkString(" ") + " в " +
      reference.source.init.reference(Language.Russian.toSpec) + ":" + reference.source.reference(Language.Russian.toSpec)
}
