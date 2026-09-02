package org.opentorah.texts.tanach

import org.opentorah.metadata.{Name, Names}
import org.podval.xml.XmlAst

private[tanach] object XmlDecode:
  export org.podval.xml.XmlDecode.{
    booleanOpt, childrenNamed, intAttr, intOpt, positiveInt, positiveIntOpt,
    requireAttr, requireName, requireNoOther
  }

  def namesOf[E: XmlAst](element: E, defaultN: Boolean = true): Names =
    val n: Option[String] = if defaultN then element.get("n") else None
    Names.fromDefaultName(n, childrenNamed(element, "name").map(Name.codec.unsafeDecode(_)))
