package org.opentorah.tei

import org.opentorah.entity.EntityReference
import org.opentorah.xml.{Attribute, Element, Parser}
import scala.xml.Elem

final case class TitleStmt(
  titles: Seq[Title.Value],
  authors: Seq[Author.Value],
  editors: Seq[Editor],
  sponsors: Seq[Sponsor.Value],
  funders: Seq[Funder.Value],
  principals: Seq[Principal.Value],
  respStmts: Seq[RespStmt.Value]
) {
  def references: Seq[EntityReference] = {
    val xml: Seq[Elem] = Seq(
      titles.map(Title.parsable.toXml) ++
      authors.map(Author.parsable.toXml) ++
      sponsors.map(Sponsor.parsable.toXml) ++
      funders.map(Funder.parsable.toXml) ++
      principals.map(Principal.parsable.toXml) ++
      respStmts.map(RespStmt.parsable.toXml)
    ).flatten

    EntityReference.from(xml) ++ editors.flatMap(_.persName.toSeq)
  }
}

object TitleStmt extends Element.WithToXml[TitleStmt]("titleStmt") {
  def apply(): TitleStmt = new TitleStmt(
    titles = Seq.empty,
    authors = Seq.empty,
    editors = Seq.empty,
    sponsors = Seq.empty,
    funders = Seq.empty,
    principals = Seq.empty,
    respStmts = Seq.empty
  )

  override protected def parser: Parser[TitleStmt] = for {
    titles <- Title.parsable.all
    authors <- Author.parsable.all
    editors <- Editor.all
    sponsors <- Sponsor.parsable.all
    funders <- Funder.parsable.all
    principals <- Principal.parsable.all
    respStmts <- RespStmt.parsable.all
  } yield new TitleStmt(
    titles,
    authors,
    editors,
    sponsors,
    funders,
    principals,
    respStmts
  )

  override protected def attributes(value: TitleStmt): Seq[Attribute.Value[_]] = Seq.empty

  override protected def content(value: TitleStmt): Seq[Elem] =
    Title.parsable.toXml(value.titles) ++
    Author.parsable.toXml(value.authors) ++
    Editor.toXml(value.editors) ++
    Sponsor.parsable.toXml(value.sponsors) ++
    Funder.parsable.toXml(value.funders) ++
    Principal.parsable.toXml(value.principals) ++
    RespStmt.parsable.toXml(value.respStmts)
}
