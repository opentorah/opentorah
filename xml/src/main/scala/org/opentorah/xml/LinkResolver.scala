package org.opentorah.xml

import org.opentorah.util.Files

trait LinkResolver {
  final def resolve(target: String): Option[LinkResolver.Resolved] = if (!target.startsWith("/")) None else {
    val (url: String, part: Option[String]) = Files.urlAndPart(target)
    resolve(url.substring(1).split("/"))
      .map(resolved => resolved.copy(url = Files.addPart(resolved.url, part)))
  }

  def resolve(url: Seq[String]): Option[LinkResolver.Resolved]

  def findByRef(ref: String): Option[LinkResolver.Resolved]

  def facs: LinkResolver.Resolved
}

object LinkResolver {

  final case class Resolved(
    url: Seq[String],
    role: Option[String]
  ) {
    def urlAsString: String = Files.mkUrl(url)

    def urlWithPartAsString(part: String): String = Files.mkUrl(Files.addPart(url, part))
  }
}
