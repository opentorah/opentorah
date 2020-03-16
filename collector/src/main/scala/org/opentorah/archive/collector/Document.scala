package org.opentorah.archive.collector

import org.opentorah.archive.collector.reference.{Reference, ReferenceSource}
import org.opentorah.reference.Entity
import org.opentorah.tei.Tei
import scala.xml.Node

final class Document(
  override val url: String,
  collection: Collection,
  val tei: Tei,
  override val name: String,
  // TODO move prev and next out of here
  val prev: Option[String],
  val next: Option[String],
  val translations: Seq[String]
) extends ReferenceSource(collection) {
  override def toString: String = s"$collection:$name"

  override val references: Seq[Reference] = bindReferences(tei.references)

  override def isNames: Boolean = false

  override def viewer: String = "documentViewer"

  val title: Option[Seq[Node]] = tei.titleStmt.titles.headOption.map(_.content)

  def authors: Seq[Seq[Node]] = tei.titleStmt.authors.map(_.xml)

  def transcribers: Seq[org.opentorah.reference.Reference] =
    tei.titleStmt.editors.filter(_.role.contains("transcriber")).flatMap(_.persName)

  def date: Option[String] = tei.creationDate.map(_.when)

  def description: Option[Seq[Node]] = tei.getAbstract.orElse(title)

  def language: Option[String] = tei.languages.map(_.ident).headOption

  val pages: Seq[Page] = for (pb <- tei.pbs) yield collection.pageType(
    n = pb.n,
    facs = pb.facs
  )

  def addressee: Option[Reference] =
    references.find(name => (name.entity == Entity.Person) && name.role.contains("addressee"))
}
