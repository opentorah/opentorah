package org.opentorah.store

import java.net.URL

sealed trait Urls {
  def fromUrl: Option[URL]

  def baseUrl: URL

  def inline: Urls .Inline
}

object Urls {

  final class FromUrl(url: URL) extends Urls {
    override def fromUrl: Option[URL] = Some(url)
    override def baseUrl: URL = url
    override def inline: Urls.Inline = new Inline(url)
  }

  final class Inline(override val baseUrl: URL) extends Urls {
    override def fromUrl: Option[URL] = None
    override def inline: Urls.Inline = this
  }

  def fromUrl(url: URL): Urls.FromUrl = new Urls.FromUrl(url)
}
