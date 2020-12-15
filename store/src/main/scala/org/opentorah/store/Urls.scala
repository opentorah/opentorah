package org.opentorah.store

import java.net.URL

sealed trait Urls {
  def fromUrl: Option[URL]

  def baseUrl: URL

  def inline: Urls.Inline
}

object Urls {

  final class FromUrl(
    override val baseUrl: URL
  ) extends Urls {
    override def fromUrl: Option[URL] = Some(baseUrl)
    override def inline: Urls.Inline = new Inline(baseUrl)
  }

  final class Inline(override val baseUrl: URL) extends Urls {
    override def fromUrl: Option[URL] = None
    override def inline: Urls.Inline = this
  }

  def fromUrl(url: URL): Urls.FromUrl = new Urls.FromUrl(url)
}
