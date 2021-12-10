package org.opentorah.tei

import org.opentorah.html.A
import zio.UIO

trait LinksResolver:
  def resolve(path: Seq[String]): UIO[Option[A]]

  def findByRef(ref: String): UIO[Option[A]]

  def facs(pageId: String): UIO[Option[A]]

object LinksResolver:
  val empty: LinksResolver = new LinksResolver:
    override def resolve(path:  Seq[String]): UIO[Option[A]] = UIO.none
    override def findByRef(ref: String): UIO[Option[A]] = UIO.none
    override def facs(pageId: String): UIO[Option[A]] = UIO.none
