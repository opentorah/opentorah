package org.opentorah.site

import org.opentorah.xml.A
import zio.{UIO, ZIO}

trait LinksResolver:
  def resolve(path: Seq[String]): UIO[Option[A]]

  def findByRef(ref: String): UIO[Option[A]]

  def facs(pageId: String): UIO[Option[A]]

object LinksResolver:
  val empty: LinksResolver = new LinksResolver:
    override def resolve(path:  Seq[String]): UIO[Option[A]] = ZIO.none
    override def findByRef(ref: String): UIO[Option[A]] = ZIO.none
    override def facs(pageId: String): UIO[Option[A]] = ZIO.none
