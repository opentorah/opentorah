package org.opentorah.tei

import org.opentorah.xml.{Unparser, Element, Parsable, Parser, Xml}

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
      Title.element.seq.unparser.content(titles) ++
      Author.element.seq.unparser.content(authors) ++
      Sponsor.element.seq.unparser.content(sponsors) ++
      Funder.element.seq.unparser.content(funders) ++
      Principal.element.seq.unparser.content(principals) ++
      RespStmt.element.seq.unparser.content(respStmts)

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

    override val unparser: Unparser[TitleStmt] = Tei.concat(
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
