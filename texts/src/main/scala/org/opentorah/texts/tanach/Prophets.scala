package org.opentorah.texts.tanach

import org.opentorah.metadata.HasName

enum Prophets(nameOverride: Option[String] = None) extends Nach(nameOverride), HasName.Enum derives CanEqual:
  // Early Prophets
  case Joshua
  case Judges
  case SamuelI extends Prophets(nameOverride = Some("I Samuel"))
  case SamuelII extends Prophets(nameOverride = Some("II Samuel"))
  case KingsI extends Prophets(nameOverride = Some("I Kings"))
  case KingsII extends Prophets(nameOverride = Some("II Kings"))

  // Late Prophets
  case Isaiah
  case Jeremiah
  case Ezekiel

  // Trei Asar
  case Hosea
  case Joel
  case Amos
  case Obadiah
  case Jonah
  case Micah
  case Nahum
  case Habakkuk
  case Zephaniah
  case Haggai
  case Zechariah
  case Malachi

