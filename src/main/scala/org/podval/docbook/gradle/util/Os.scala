package org.podval.docbook.gradle.util

sealed trait Os {
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
  case object Linux extends Os
  case object Android extends Os
}
