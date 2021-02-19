package org.opentorah.html

import org.opentorah.xml.Xml

final class EndNote(
  number: Int,
  id: Option[String],
  val content: Xml.Nodes
) {
  private def contentId: String = s"_note_$number"

  private def srcId: String = id.getOrElse(s"src_note_$number")

  def link: Xml.Element =
    a().setFragment(contentId).setId(srcId)(element = <sup>{number}</sup>)

  // TODO is HTML namespace here needed?
  def body: Xml.Element =
    <span xmlns={Html.namespace.uri} class="endnote" id={contentId}>
      {a().setFragment(srcId).addClass("endnote-backlink")(number.toString)}
      {content}
    </span>
}
