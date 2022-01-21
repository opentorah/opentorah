package org.opentorah.platform

enum Os(
  val hasUname: Boolean = false,
  val libraryExtension: String = "so"
) derives CanEqual:
  case Windows extends Os(libraryExtension = "dll")
  case Aix
  case Mac extends Os(libraryExtension = "dylib")
  case FreeBSD
  case SunOS
  case Linux extends Os(hasUname = true)
  case Android

object Os:
  def getName: String = System.getProperty("os.name")

  def get: Os =
    val name: String = getName.toLowerCase
    val result = Os.values
      .find(_.toString.toLowerCase.contains(name.toLowerCase))
      .getOrElse(throw IllegalArgumentException(s"Unsupported OS: $name"))
    if result == Os.Linux && System.getProperty("java.specification.vendor").contains("Android") then Os.Android
    else result
