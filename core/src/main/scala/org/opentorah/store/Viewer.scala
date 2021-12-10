package org.opentorah.store

import org.opentorah.html.Html

object Viewer:

  trait Hierarchy extends Store:
    final override def viewer: String = hierarchy

  trait Default extends Store:
    final override def viewer: String = default

  trait Text extends Store:
    final override def viewer: String = "textViewer"

  trait Facsimile extends Store:
    final override def viewer: String = facsimile

  trait Apparatus extends Store:
    final override def viewer: String = "apparatusViewer"

  def default: String = hierarchy

  val hierarchy: String = Html.viewerDefault

  val facsimile: String = "facsimileViewer"
