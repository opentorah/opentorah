package org.podval.fop.util

object Util {

  def mapValues[A, B, C](map: Map[A, B])(f: B => C): Map[A, C] =
    map.view.mapValues(f).toMap // Scala 2.13
    //map.mapValues(f) // Scala 2.12

  def applicationString: String = {
    val info = getClass.getPackage
    info.getImplementationTitle + " version " + info.getImplementationVersion
  }
}
