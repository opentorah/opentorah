package org.opentorah.store

import java.net.URL
import org.opentorah.util.Files

sealed trait Urls {
  def fromUrl: Option[URL]

  def baseUrl: URL

  def inline: Urls.Inline
}

object Urls {

  final class FromUrl(
    override val baseUrl: URL,
    val redirects: Seq[Redirect] = Seq.empty
  ) extends Urls {
    override def fromUrl: Option[URL] = Some(baseUrl)
    override def inline: Urls.Inline = new Inline(baseUrl)

    def redirect(file: String): FromUrl = new FromUrl(
      baseUrl = Files.fileInDirectory(baseUrl, file),
      redirects = redirects :+ new Redirect(baseUrl, file)
    )
  }

  final class Inline(override val baseUrl: URL) extends Urls {
    override def fromUrl: Option[URL] = None
    override def inline: Urls.Inline = this
  }

  def fromUrl(url: URL): Urls.FromUrl = new Urls.FromUrl(url)
}
