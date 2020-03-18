package org.opentorah.archive.collector

import java.net.URL
import org.opentorah.metadata.{LanguageSpec, Name, Names}
import org.opentorah.reference.{Entity, Reference}
import org.opentorah.store.{By, Nameds, Path, Selector}
import org.opentorah.tei.Tei
import scala.xml.Node

final class Document(
  val url: URL,
  val pageType: Page.Type,
  val tei: Tei,
  val name: String,
  val translations: Seq[String]
) extends org.opentorah.store.Store {

  override def toString: String = name

  override def names: Names = new Names(Seq(new Name(name, LanguageSpec.empty)))

  override def selectors: Seq[Selector] = Seq.empty

  override def nameds: Option[Nameds] = None

  override def by: Option[By] = None

  override def references(at: Path): Seq[Reference] = tei.references.map(_.at(at))

  def addressee: Option[Reference] =
    references(Path.empty).find(name => (name.entity == Entity.Person) && name.role.contains("addressee"))

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
}
