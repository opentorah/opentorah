package org.opentorah.xml

import org.opentorah.util.Files

trait LinkResolver {
  // TODO maybe just call up?
  final def resolve(target: String): Option[Html.a] = if (!target.startsWith("/")) None else {
    val (url: String, part: Option[String]) = Files.urlAndPart(target)
    resolve(Files.splitUrl(url))
      .map(a => a.copy(part = part))
  }

  def resolve(url: Seq[String]): Option[Html.a]

  def findByRef(ref: String): Option[Html.a]

  def facs(pageId: String): Option[Html.a]
}
