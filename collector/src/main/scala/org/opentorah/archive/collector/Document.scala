package org.opentorah.archive.collector

import java.net.URL
import org.opentorah.reference.{Entity, Reference}
import org.opentorah.store.Path
import org.opentorah.tei.Tei
import scala.xml.Node

final class Document(
  val url: URL,
  val path: Path,
  val pageType: Page.Type,
  val tei: Tei,
  val name: String,
  // TODO move prev and next out of here
  val prev: Option[String],
  val next: Option[String],
  val translations: Seq[String]
) {
  val references: Seq[Reference] = tei.references.map(_.at(path))

  val title: Option[Seq[Node]] = tei.titleStmt.titles.headOption.map(_.content)

  def authors: Seq[Seq[Node]] = tei.titleStmt.authors.map(_.xml)

  def transcribers: Seq[org.opentorah.reference.Reference] =
    tei.titleStmt.editors.filter(_.role.contains("transcriber")).flatMap(_.persName)

  def date: Option[String] = tei.creationDate.map(_.when)

  def description: Option[Seq[Node]] = tei.getAbstract.orElse(title)

  def language: Option[String] = tei.languages.map(_.ident).headOption

  val pages: Seq[Page] = for (pb <- tei.pbs) yield pageType(
    n = pb.n,
    facs = pb.facs
  )

  def addressee: Option[Reference] =
    references.find(name => (name.entity == Entity.Person) && name.role.contains("addressee"))
}
