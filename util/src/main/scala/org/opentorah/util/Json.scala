package org.opentorah.util

object Json {

  def fromMap(map: Map[String, Any]): String =
    "{\n" + (for ((key, value) <- map.toSeq) yield "\"" + key + "\":" + from(value)).mkString(",\n") + "\n}"

  private def fromList(list: List[Any]): String =
    "[" + list.map(from).mkString(", ") + "]"

  private def from(value: Any): String = value match {
    // with value: Map[String, Any] I get:
    //   non-variable type argument String in type pattern scala.collection.immutable.Map[String,Any]
    //   (the underlying of Map[String,Any]) is unchecked since it is eliminated by erasure
    case value: Map[_, Any] => fromMap(value.asInstanceOf[Map[String, Any]])
    case value: List[Any] => fromList(value)
    case value: String => "\"" + fromString(value) + "\""
    case other => other.toString
  }

  def fromString(value: String): String = value
    .replace("\\", "\\\\") // first, so that newly-introduced '\' do not get escaped!
    .replace("\"", "\\\"")
    .replace("\n", "\\n")

}
