package org.opentorah.xml

import java.net.URL

final class FromUrl(
  val url: URL,
  val inline: Boolean
)

object FromUrl:

  trait With:
    def fromUrl: FromUrl
