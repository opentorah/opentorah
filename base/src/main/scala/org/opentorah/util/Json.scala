package org.opentorah.util

object Json:

  def fromMap(map: Map[String, Matchable]): String =
    "{\n" + (for (key, value) <- map.toSeq yield "\"" + key + "\":" + from(value)).mkString(",\n") + "\n}"

  private def fromList(list: List[Matchable]): String =
    "[" + list.map(from).mkString(", ") + "]"

  private def from(value: Matchable): String = value match
    // with value: Map[String, Matchable] I get:
    //   non-variable type argument String in type pattern scala.collection.immutable.Map[String,Matchable]
    //   (the underlying of Map[String,Matchable]) is unchecked since it is eliminated by erasure
    case value: Map[?, ?] => fromMap(value.asInstanceOf[Map[String, Matchable]])
    case value: List[?] => fromList(value.asInstanceOf[List[Matchable]])
    case value: String => "\"" + Strings.escape(value) + "\""
    case other => other.toString

  def optionToJs(value: Option[String]): String =
    value.fold("null")(value => s"'$value'")
