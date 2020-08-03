package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parser}
import scala.xml.Node

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
    val xml: Seq[Node] =
      Title.parsable.toXmlSeq.content(titles) ++
      Author.parsable.toXmlSeq.content(authors) ++
      Sponsor.parsable.toXmlSeq.content(sponsors) ++
      Funder.parsable.toXmlSeq.content(funders) ++
      Principal.parsable.toXmlSeq.content(principals) ++
      RespStmt.parsable.toXmlSeq.content(respStmts)

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

  override protected val parser: Parser[TitleStmt] = for {
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

  override protected val antiparser: Antiparser[TitleStmt] = Antiparser(
    Title.parsable.toXmlSeq.compose(_.titles),
    Author.parsable.toXmlSeq.compose(_.authors),
    Editor.toXmlSeq.compose(_.editors),
    Sponsor.parsable.toXmlSeq.compose(_.sponsors),
    Funder.parsable.toXmlSeq.compose(_.funders),
    Principal.parsable.toXmlSeq.compose(_.principals),
    RespStmt.parsable.toXmlSeq.compose(_.respStmts)
  )
}
