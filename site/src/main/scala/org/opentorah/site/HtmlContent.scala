package org.opentorah.site

import org.opentorah.html
import org.opentorah.store.{Caching, Store}
import org.opentorah.xml.ScalaXml

// TODO this should be part of the Store!
trait HtmlContent[S <: Site[S]]:
  def htmlHeadTitle: Option[String]

  def htmlBodyTitle: Option[ScalaXml.Nodes] = None

  def content(path: Store.Path, site: S): Caching.Parser[ScalaXml.Element]

  def style: String = "main"

  def viewer: String

object HtmlContent:
  final def a(path: Store.Path): html.a = html
    .a(Store.structureNames(path))
    .setTarget(path.last.asInstanceOf[HtmlContent[?]].viewer)

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

