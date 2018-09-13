package org.podval.calendar.metadata

trait Named extends HasNames {
  def name: String = Named.className(this)

  override def toString: String = name
}

object Named {
  def className(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")
}
