package org.podval.fop.util

object Util {

  // TODO use it from opentorah-util.Collections:
  def mapValues[A, B, C](map: Map[A, B])(f: B => C): Map[A, C] =
    // map.view.mapValues(f).toMap // TODO for Scala 2.13
      map.mapValues(f) // Scala 2.12

  // TODO move into util.Util
  def applicationString: String = {
    val info = getClass.getPackage
    info.getImplementationTitle + " version " + info.getImplementationVersion
  }
}
