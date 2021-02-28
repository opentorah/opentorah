package org.opentorah.texts.tanach

trait TreiAsar extends LateProphets

object TreiAsar {

  case object Hosea extends TreiAsar
  case object Joel extends TreiAsar
  case object Amos extends TreiAsar
  case object Obadiah extends TreiAsar
  case object Jonah extends TreiAsar
  case object Micah extends TreiAsar
  case object Nahum extends TreiAsar
  case object Habakkuk extends TreiAsar
  case object Zephaniah extends TreiAsar
  case object Haggai extends TreiAsar
  case object Zechariah extends TreiAsar
  case object Malachi extends TreiAsar

  val all: Seq[TreiAsar] = Seq(Hosea, Joel, Amos, Obadiah, Jonah, Micah,
    Nahum, Habakkuk, Zephaniah, Haggai, Zechariah, Malachi)
}
