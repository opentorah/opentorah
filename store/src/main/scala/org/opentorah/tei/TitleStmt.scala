package org.opentorah.tei

import org.opentorah.reference.Reference
import org.opentorah.xml.Descriptor
import scala.xml.{Elem, Node}

final case class TitleStmt(
  titles: Seq[Title],
  authors: Seq[Author],
  editors: Seq[Editor],
  sponsors: Seq[Sponsor],
  funders: Seq[Funder],
  principals: Seq[Principal],
  respStmts: Seq[RespStmt]
) {
  def references: Seq[Reference] = {
    val xml: Seq[Node] = Seq(titles.flatMap(_.content) ++
      authors.map(Author.toXml) ++
      editors.flatMap(_.persName.toSeq) ++
      sponsors.map(Sponsor.toXml) ++
      funders.map(Funder.toXml) ++
      principals.map(Principal.toXml) ++
      respStmts.map(RespStmt.toXml)
    ).flatten

    xml.flatMap(Reference.all)
  }
}

object TitleStmt extends Descriptor[TitleStmt](
  elementName = "titleStmt",
  contentParser = for {
    titles <- Title.all
    authors <- Author.all
    editors <- Editor.all
    sponsors <- Sponsor.all
    funders <- Funder.all
    principals <- Principal.all
    respStmts <- RespStmt.all
  } yield new TitleStmt(
    titles,
    authors,
    editors,
    sponsors,
    funders,
    principals,
    respStmts
  )
) {
  def apply(): TitleStmt = new TitleStmt(
    titles = Seq.empty,
    authors = Seq.empty,
    editors = Seq.empty,
    sponsors = Seq.empty,
    funders = Seq.empty,
    principals = Seq.empty,
    respStmts = Seq.empty
  )

  override def toXml(value: TitleStmt): Elem =
    <titleStmt>
      {Title.toXml(value.titles)}
      {Author.toXml(value.authors)}
      {Editor.toXml(value.editors)}
      {Sponsor.toXml(value.sponsors)}
      {Funder.toXml(value.funders)}
      {Principal.toXml(value.principals)}
      {RespStmt.toXml(value.respStmts)}
    </titleStmt>
}
