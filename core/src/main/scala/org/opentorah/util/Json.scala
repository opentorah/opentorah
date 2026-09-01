package org.opentorah.util

object Json:

  def fromMap(map: Map[String, Matchable]): String =
    "{\n" + (for (key, value) <- map.toSeq yield "\"" + key + "\":" + from(value)).mkString(",\n") + "\n}"

  private def fromList(list: List[Matchable]): String =
    "[" + list.map(from).mkString(", ") + "]"

  private def from(value: Matchable): String = value match
    case value: Map[?, ?] => fromMap(value.asInstanceOf[Map[String, Matchable]])
    case value: List[?] => fromList(value.asInstanceOf[List[Matchable]])
    case value: String => "\"" + Strings.escape(value) + "\""
    case other => other.toString

  def optionToJs(value: Option[String]): String =
    value.fold("null")(stringToJs)
    
  def stringToJs(value: String): String = s"'$value'"
