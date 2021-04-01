package org.opentorah.util

sealed trait Os {
  final def name: String = Util.className(this)
  def hasUname: Boolean = false
  def libraryExtension: String = "so"
}

object Os {
  case object Windows extends Os {
    override def libraryExtension: String = "dll"
  }

  case object Aix extends Os

  case object Mac extends Os {
    override def libraryExtension: String = "dylib"
  }

  case object FreeBSD extends Os

  case object SunOS extends Os

  case object Linux extends Os {
    override def hasUname: Boolean = true
  }

  case object Android extends Os

  val values: Seq[Os] = Seq(Windows, Aix, Mac, FreeBSD, SunOS, Linux, Android)
}
