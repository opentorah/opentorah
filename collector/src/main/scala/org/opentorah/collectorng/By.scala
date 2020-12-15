package org.opentorah.collectorng

import org.opentorah.xml.FromUrl

trait By extends FromUrl.With {
  def selector: Selector

  def resolve(url: Seq[String]): Option[SiteFile] =
    if (selector.resolves(url)) resolveStore(url.tail) else None

  protected def resolveStore(url: Seq[String]): Option[SiteFile]
}
