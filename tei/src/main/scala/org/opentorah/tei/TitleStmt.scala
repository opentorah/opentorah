package org.opentorah.tei

import org.opentorah.xml.{Unparser, Element, Parsable, Parser}

final class TitleStmt(
  val titles: Seq[Title.Value],
  val authors: Seq[Author.Value],
  val editors: Seq[Editor],
  val sponsors: Seq[Sponsor.Value],
  val funders: Seq[Funder.Value],
  val principals: Seq[Principal.Value],
  val respStmts: Seq[RespStmt.Value]
)

object TitleStmt extends Element[TitleStmt]("titleStmt"):

  def empty: TitleStmt = TitleStmt(
    titles = Seq.empty,
    authors = Seq.empty,
    editors = Seq.empty,
    sponsors = Seq.empty,
    funders = Seq.empty,
    principals = Seq.empty,
    respStmts = Seq.empty
  )

  override def contentParsable: Parsable[TitleStmt] = new Parsable[TitleStmt]:
    override val parser: Parser[TitleStmt] = for
      titles: Seq[Title.Value] <- Title.element.seq()
      authors: Seq[Author.Value] <- Author.element.seq()
      editors: Seq[Editor] <- Editor.seq()
      sponsors: Seq[Sponsor.Value] <- Sponsor.element.seq()
      funders: Seq[Funder.Value] <- Funder.element.seq()
      principals: Seq[Principal.Value] <- Principal.element.seq()
      respStmts: Seq[RespStmt.Value] <- RespStmt.element.seq()
    yield TitleStmt(
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
