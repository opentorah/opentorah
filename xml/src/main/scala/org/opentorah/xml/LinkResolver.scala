package org.opentorah.xml

trait LinkResolver {
  def resolve(url: Seq[String]): Option[Html.a]

  def findByRef(ref: String): Option[Html.a]

  def facs(pageId: String): Option[Html.a]
}
