package org.opentorah.texts.tanach

private[tanach] object XmlDecode:
  export org.podval.xml.XmlDecode.{
    booleanOpt, childrenNamed, intAttr, intOpt, positiveInt, positiveIntOpt,
    requireAttr, requireName, requireNoOther
  }
