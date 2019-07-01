package org.podval.docbook.gradle.util

object Json {

  def fromMap(map: Map[String, Any]): String =
    "{\n" + (for ((key, value) <- map.toSeq) yield "\"" + key + "\":" + from(value)).mkString(",\n") + "\n}"

  private def fromList(list: List[Any]): String =
    "[" + list.map(from).mkString(", ") + "]"

  private def from(value: Any): String = value match {
    case value: Map[String, Any] => fromMap(value)
    case value: List[Any] => fromList(value)
    case value: String => "\"" + fromString(value) + "\""
    case other => other.toString
  }

  def fromString(value: String): String = value
    .replace("\\", "\\\\") // first, so that newly-introduced '\' do not get escaped!
    .replace("\"", "\\\"")
    .replace("\n", "\\n")

}
