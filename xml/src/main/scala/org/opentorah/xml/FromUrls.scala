package org.opentorah.xml

import java.net.URL

sealed trait FromUrls {
  def baseUrl: URL
}

object FromUrls {

  final case class Top(
    override val baseUrl: URL,
    redirectedFrom: Seq[URL]
  ) extends FromUrls

  final case class Nested(
    override val baseUrl: URL
  ) extends FromUrls
}
