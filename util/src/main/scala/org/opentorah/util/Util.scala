package org.opentorah.util

object Util {
  // Maybe in JDK 9 and later I won't need to deal with '$'?
  // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8057919
  def className(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")

  def applicationString: String = {
    val info = getClass.getPackage
    info.getImplementationTitle + " version " + info.getImplementationVersion
  }

  def split(what: String, on: Char): (String, Option[String]) = what.lastIndexOf(on) match {
    case -1 => (what, None)
    case index => (what.substring(0, index), Some(what.substring(index+1)))
  }

  def splitRight(what: String, on: Char): (Option[String], String) = what.lastIndexOf(on) match {
    case -1 => (None, what)
    case index => (Some(what.substring(0, index)), what.substring(index+1))
  }

  def squashWhitespace(what: String): String = what
    .replace('\n', ' ')
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
    .replace("  ", " ")
}
