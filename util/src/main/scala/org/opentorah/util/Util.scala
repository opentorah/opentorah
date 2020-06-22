package org.opentorah.util

object Util {
  // Maybe in JDK 9 and later I won't need to deal with '$'?
  // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8057919
  def className(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")

  def applicationString: String = {
    val info = getClass.getPackage
    info.getImplementationTitle + " version " + info.getImplementationVersion
  }

  def split(what: String, on: Char): (String, Option[String]) = {
    val index: Int = what.lastIndexOf(on)
    if (index == -1) (what, None)
    else (what.substring(0, index), Some(what.substring(index+1)))
  }
}
