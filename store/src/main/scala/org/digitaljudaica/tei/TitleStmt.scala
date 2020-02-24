package org.digitaljudaica.tei

import org.digitaljudaica.reference.Reference
import org.digitaljudaica.xml.Descriptor
import scala.xml.Node

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
      authors.map(_.xml) ++
      editors.flatMap(_.persName.toSeq) ++
      sponsors.map(_.xml) ++
      funders.map(_.xml) ++
      principals.map(_.xml) ++
      respStmts.map(_.xml)
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
)
