package org.podval.calendar.metadata

// TODO merge with Name used for Months etc.
final case class Name(
  name: String,
  lang: Option[String],
  isTransliterated: Option[Boolean],
  flavour: Option[String]
)
