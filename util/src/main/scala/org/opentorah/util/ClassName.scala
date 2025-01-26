package org.opentorah.util

object ClassName:
  // TODO after Java 9, since https://bugs.java.com/bugdatabase/view_bug.do?bug_id=JDK-8057919
  // was fixed, I may not need to do anything special about the '$'...
  // Test it
  def get(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")
