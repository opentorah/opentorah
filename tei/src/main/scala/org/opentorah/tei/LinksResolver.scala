package org.opentorah.tei

import org.opentorah.html
import zio.UIO

trait LinksResolver {
  def resolve(path: Seq[String]): UIO[Option[html.a]]

  def findByRef(ref: String): UIO[Option[html.a]]

  def facs(pageId: String): UIO[Option[html.a]]
}

object LinksResolver {
  val empty: LinksResolver = new LinksResolver {
    override def resolve(path:  Seq[String]): UIO[Option[html.a]] = UIO.none
    override def findByRef(ref: String): UIO[Option[html.a]] = UIO.none
    override def facs(pageId: String): UIO[Option[html.a]] = UIO.none
  }
}
