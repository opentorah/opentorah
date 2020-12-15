package org.opentorah.collector

import org.opentorah.tei.{Body, Tei}
import org.opentorah.xml.{LinkResolver, Xml}

trait TeiSiteFile extends SiteFile {

  final override protected def titleAndContent: TitleAndContent = {
    val tei: Tei = this.tei
    val withSummary: Tei = tei.copy(text = tei.text.copy(body = new Body.Value(headerSummary ++ tei.body.xml)))
    val result: Tei = teiTransformer(withSummary)

    new TitleAndContent(
      title, // TODO also tei.titleStmt.titles,
      content = Tei.toHtml(resolver, result)
    )
  }

  protected def title: Option[String]

  protected def tei: Tei

  protected def headerSummary: Seq[Xml.Node]

  protected def teiTransformer: Tei.Transformer

  protected def resolver: LinkResolver
}
