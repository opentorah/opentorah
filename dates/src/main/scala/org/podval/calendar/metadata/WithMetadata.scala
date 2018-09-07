package org.podval.calendar.metadata

trait WithMetadata[M <: Metadata] {
  def name: String = {
    // TODO de-camelCase - or do nothing?
    val result =
//      if (className.endsWith("II")) className.dropRight(2) ++ " II" else
//      if (className.endsWith("I")) className.dropRight(1) ++ " I" else
        WithMetadata.className(this)

    result
  }

  def metadata: M

  final def names: Names = metadata.names
}

object WithMetadata {
  def className(obj: AnyRef): String = obj.getClass.getSimpleName.replace("$", "")
}