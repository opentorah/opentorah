package org.opentorah.util

object Util {
  // Maybe in JDK 9 and later I won't need to deal with '$'?
  // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8057919
  def className(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")

  def getSingleton(className: String): AnyRef = Class.forName(className + "$")
    .getField("MODULE$")
    .get(null)

  def getSingletonClassName(obj: AnyRef): String = ???

  def applicationString: String = {
    val info = getClass.getPackage
    info.getImplementationTitle + " version " + info.getImplementationVersion
  }
}
