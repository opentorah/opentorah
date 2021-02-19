package org.opentorah.tei

import org.opentorah.html.a
import zio.{Has, URIO}

trait LinksResolver {
  def resolve(path: Seq[String]): Option[a]

  def findByRef(ref: String): Option[a]

  def facs(pageId: String): Option[a]
}

object LinksResolver {

  def resolve(path: Seq[String]): URIO[Has[LinksResolver], Option[a]] =
    URIO.access(_.get.resolve(path))

  def findByRef(ref: String): URIO[Has[LinksResolver], Option[a]] =
    URIO.access(_.get.findByRef(ref))

  def facs(pageId: String): URIO[Has[LinksResolver], Option[a]] =
    URIO.access(_.get.facs(pageId))
}
