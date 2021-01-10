package org.opentorah.tei

import org.opentorah.xml.{Antiparser, Element, Parsable, Parser, Xml}

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
    val xml: Seq[Xml.Node] =
      Title.element.seq.antiparser.content(titles) ++
      Author.element.seq.antiparser.content(authors) ++
      Sponsor.element.seq.antiparser.content(sponsors) ++
      Funder.element.seq.antiparser.content(funders) ++
      Principal.element.seq.antiparser.content(principals) ++
      RespStmt.element.seq.antiparser.content(respStmts)

    EntityReference.from(xml) ++ editors.flatMap(_.persName.toSeq)
  }
}

object TitleStmt extends Element[TitleStmt]("titleStmt") {
  def empty: TitleStmt = new TitleStmt(
    titles = Seq.empty,
    authors = Seq.empty,
    editors = Seq.empty,
    sponsors = Seq.empty,
    funders = Seq.empty,
    principals = Seq.empty,
    respStmts = Seq.empty
  )

  override def contentParsable: Parsable[TitleStmt] = new Parsable[TitleStmt] {
    override val parser: Parser[TitleStmt] = for {
      titles <- Title.element.seq()
      authors <- Author.element.seq()
      editors <- Editor.seq()
      sponsors <- Sponsor.element.seq()
      funders <- Funder.element.seq()
      principals <- Principal.element.seq()
      respStmts <- RespStmt.element.seq()
    } yield new TitleStmt(
      titles,
      authors,
      editors,
      sponsors,
      funders,
      principals,
      respStmts
    )

    override val antiparser: Antiparser[TitleStmt] = Tei.concat(
      Title.element.seq(_.titles),
      Author.element.seq(_.authors),
      Editor.seq(_.editors),
      Sponsor.element.seq(_.sponsors),
      Funder.element.seq(_.funders),
      Principal.element.seq(_.principals),
      RespStmt.element.seq(_.respStmts)
    )
  }
}
