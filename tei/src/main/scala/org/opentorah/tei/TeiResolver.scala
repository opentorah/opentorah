package org.opentorah.tei

trait TeiResolver {
  def resolve(url: String): Option[TeiResolver.Resolved]

  def findByRef(ref: String): Option[TeiResolver.Resolved]

  def facs: TeiResolver.Resolved
}

object TeiResolver {

  final class Resolved(
    val url: Seq[String],
    val role: Option[String]
  )
}
