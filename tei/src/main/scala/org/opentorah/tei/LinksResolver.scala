package org.opentorah.tei

import org.opentorah.html
import zio.UIO

trait LinksResolver {
  def resolve(path: Seq[String]): UIO[Option[html.a]]

  def findByRef(ref: String): UIO[Option[html.a]]

  def facs(pageId: String): UIO[Option[html.a]]
}
