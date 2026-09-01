package org.opentorah.xml

import java.net.URL

final class FromUrl(
  val url: URL
)

object FromUrl:
  def get: Parser[FromUrl] = ParserState.access(_.fromUrl)

  trait With:
    def fromUrl: FromUrl
