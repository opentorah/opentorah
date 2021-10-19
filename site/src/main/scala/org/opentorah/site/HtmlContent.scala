package org.opentorah.site

import org.opentorah.html
import org.opentorah.store.Path
import org.opentorah.xml.{Caching, ScalaXml}

// TODO should this be a part of the Store?
trait HtmlContent[S <: Site[S]]:
  def htmlHeadTitle: Option[String]

  def htmlBodyTitle: Option[ScalaXml.Nodes] = None

  def content(path: Path, site: S): Caching.Parser[ScalaXml.Element]

  // TODO split into prev, next, up and more?
  def navigationLinks(path: Path, site: S): Caching.Parser[Seq[ScalaXml.Element]] = zio.ZIO.succeed (Seq.empty)

  def style: String = "main"

  def viewer: String

object HtmlContent:
  trait Wide:
    self: HtmlContent[?] =>
    final override def style: String = "wide"
  
  trait HierarchyViewer[S <: Site[S]] extends HtmlContent[S]:
    final override def viewer: String = hierarchyViewer

  trait DefaultViewer[S <: Site[S]] extends HierarchyViewer[S]

  trait TextViewer[S <: Site[S]] extends HtmlContent[S]:
    final override def viewer: String = "textViewer"

  trait FacsimileViewer[S <: Site[S]] extends HtmlContent[S]:
    final override def viewer: String = facsimileViewer

  trait ApparatusViewer[S <: Site[S]] extends HtmlContent[S]:
    final override def viewer: String = "apparatusViewer"

  val hierarchyViewer: String =  "hierarchyViewer"

  val facsimileViewer: String =  "facsimileViewer"

