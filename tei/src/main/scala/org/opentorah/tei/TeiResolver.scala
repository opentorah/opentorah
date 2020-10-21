package org.opentorah.tei

import org.opentorah.util.Files

trait TeiResolver {
  final def resolve(target: String): Option[TeiResolver.Resolved] = if (!target.startsWith("/")) None else {
    val (url: String, part: Option[String]) = Files.urlAndPart(target)
    resolve(url.substring(1).split("/"))
      .map(resolved => resolved.copy(url = Files.addPart(resolved.url, part)))
  }

  def resolve(url: Seq[String]): Option[TeiResolver.Resolved]

  def findByRef(ref: String): Option[TeiResolver.Resolved]

  def facs: TeiResolver.Resolved
}

object TeiResolver {

  final case class Resolved(
    url: Seq[String],
    role: Option[String]
  )
}
