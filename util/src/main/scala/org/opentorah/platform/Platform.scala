package org.opentorah.platform

object Platform:
  // Maybe in JDK 9 and later I won't need to deal with '$'?
  // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8057919
  // TODO write a test
  def className(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")

  def applicationString: String =
    val info = getClass.getPackage
    info.getImplementationTitle + " version " + info.getImplementationVersion
