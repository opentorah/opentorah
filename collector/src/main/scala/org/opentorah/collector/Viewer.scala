package org.opentorah.collector

import org.opentorah.site.Viewer

object Viewer:
  case object Collection extends Viewer("collectionViewer")
  case object Document   extends Viewer("documentViewer"  )
  case object Names      extends Viewer("namesViewer"     )
  case object Facsimile  extends Viewer("facsimileViewer" )

  val default: Viewer = Collection
