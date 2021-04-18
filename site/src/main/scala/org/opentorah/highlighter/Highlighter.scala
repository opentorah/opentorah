package org.opentorah.highlighter

import org.opentorah.xml.Xml
import scala.xml.Elem

trait Highlighter {
  def head: Seq[Xml.Element]

  def body: Seq[Xml.Element]
}

object Highlighter {

  def get(usePrism: Boolean): Highlighter =
    if (usePrism) Prism else Highlight


  // https://highlightjs.org/usage/
  // to individually load additional languages:
  //   <script src="https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@10.7.2/build/languages/go.min.js"/>
  private object Highlight extends Highlighter {
    val version: String = "10.7.2"

    override def head: Seq[Xml.Element] = Seq(
      <link
        rel="stylesheet"
        href={s"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@$version/build/styles/default.min.css"}/>,
    )

    override def body: Seq[Xml.Element] = Seq(
      <script
        src={s"https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@$version/build/highlight.min.js"}/>,
      <script>hljs.highlightAll();</script>
    )
  }

  // https://prismjs.com/#basic-usage
  private object Prism extends Highlighter {
    val version: String = "1.23.0"

    override def head: Seq[Elem] = Seq(
      <link
        rel="stylesheet"
        href={s"https://cdn.jsdelivr.net/npm/prismjs@$version/themes/prism.css"}/>
    )

    override def body: Seq[Elem] = Seq(
      <script
        src={s"https://cdn.jsdelivr.net/npm/prismjs@$version/components/prism-core.min.js"}/>,
      <script
        src={s"https://cdn.jsdelivr.net/npm/prismjs@$version/plugins/autoloader/prism-autoloader.min.js"}/>
    )
  }
}
